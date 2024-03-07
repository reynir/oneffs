open Lwt.Syntax

module Header = struct
  type t = {
    length : int;
    file_crc : Checkseum.Crc32.t;
  }
  (* structure:
       magic header '\x1f\x1f'
       file length : uint64;
       file_crc32 : uint32;
       header_crc32 : uint32;
       reserved : remaining space of sector
  *)

  let magic = 0x1F1F

  let digest_size = 4

  let length = 2 + 8 + digest_size + digest_size

  let empty = "\x1f\x1f" ^ String.init (length - 2) (Fun.const '\000')

  let create data =
    let length = String.length data in
    let file_crc = Checkseum.Crc32.digest_string data 0 length Checkseum.Crc32.default in
    { length; file_crc }

  let unmarshal buf =
    if Cstruct.length buf < length then raise (Invalid_argument "Header.unmarshal: Buffer too short");
    let magic' = Cstruct.BE.get_uint16 buf 0 in
    let data_length = Cstruct.BE.get_uint64 buf 2 in
    let file_crc = Optint.of_unsigned_int32 (Cstruct.BE.get_uint32 buf (2 + 8)) in
    let crc = Optint.of_unsigned_int32 (Cstruct.BE.get_uint32 buf (2 + 8 + digest_size)) in
    let crc' =
      Checkseum.Crc32.digest_bigstring buf.buffer buf.off
        (2 + 8 + digest_size) Checkseum.Crc32.default
    in
    (* XXX: check whole buffer is zero? *)
    if (magic' = 0 || magic' = magic) &&
       data_length = 0L && Optint.(equal zero file_crc) && Optint.(equal zero crc) then
      Ok None (* if it's all zeroed we treat it as empty *)
    else if magic' <> magic then
      Error "Not a OneFFS"
    else if not (Checkseum.Crc32.equal crc crc') then
      Error "Bad CRC"
    else
      match Int64.unsigned_to_int data_length with
      | Some length when length >= 0 ->
        Ok (Some { length; file_crc })
      | _ ->
        Error "Length too long"

  let marshal t buf =
    if Cstruct.length buf < length then raise (Invalid_argument "Header.marshal: Buffer too short");
    Cstruct.BE.set_uint16 buf 0 magic;
    Cstruct.BE.set_uint64 buf 2 (Int64.of_int t.length);
    Cstruct.BE.set_uint32 buf (2 + 8) (Optint.to_int32 t.file_crc);
    let crc = Checkseum.Crc32.digest_bigstring buf.buffer buf.off (2 + 8 + digest_size) Checkseum.Crc32.default in
    Cstruct.BE.set_uint32 buf (2 + 8 + digest_size) (Optint.to_int32 crc)
end

module Make(B : Mirage_block.S) = struct
  type t = {
    b : B.t;
    info : Mirage_block.info;
    mutable f : Header.t option;
    empty_header : Cstruct.t;
  }

  type error = [ `Block of B.error | `Bad_checksum ]

  type write_error = B.write_error

  let pp_error ppf = function
    | `Block e -> B.pp_error ppf e
    | `Bad_checksum -> Fmt.pf ppf "Bad checksum"

  let pp_write_error = B.pp_write_error

  let is_set t = Option.is_some t.f

  let write t s =
    let (let*?) = Lwt_result.bind in
    (* First invalidate current file *)
    t.f <- None;
    let*? () =
      B.write t.b 0L [t.empty_header]
    in
    let sectors =
      let sector_size = Int64.of_int t.info.sector_size in
      Int64.(to_int (div (add (of_int (String.length s)) (pred sector_size))
                       sector_size))
    in
    let buf = Cstruct.create (succ sectors * t.info.sector_size) in
    Cstruct.blit_from_string s 0 buf t.info.sector_size (String.length s);
    let bufs = List.init sectors (fun i -> Cstruct.sub buf (succ i * t.info.sector_size) t.info.sector_size) in
    let*? () = B.write t.b 1L bufs in
    let header = Header.create s in
    let buf = Cstruct.sub buf 0 t.info.sector_size in
    Header.marshal header buf;
    let*? () = B.write t.b 0L [buf] in
    t.f <- Some header;
    Lwt_result.return ()

  let read t =
    match t.f with
    | None -> Lwt_result.return None
    | Some { Header.length; file_crc } ->
      let sector_size = t.info.Mirage_block.sector_size in
      let sectors =
        let sector_size = Int64.of_int sector_size in
        Int64.(to_int (div (add (of_int length) (pred sector_size)) sector_size))
      in
      let buf = Cstruct.create (sectors * sector_size) in
      let bufs =
        List.init sectors
          (fun i -> Cstruct.sub buf (i * sector_size) sector_size)
      in
      let* r = B.read t.b 1L bufs in
      match r with
      | Error e -> Lwt_result.fail (`Block e)
      | Ok () ->
        let crc =
          Checkseum.Crc32.digest_bigstring buf.buffer buf.off length
            Checkseum.Crc32.default
        in
        if Optint.equal crc file_crc then
          let s = Cstruct.to_string ~len:length buf in
          Lwt_result.return (Some s)
        else
          Lwt_result.fail `Bad_checksum

  let format b =
    let* info = B.get_info b in
    let buf = Cstruct.create info.sector_size in
    Cstruct.blit_from_string Header.empty 0 buf 0 (String.length Header.empty);
    B.write b 0L [buf]

  let size t =
    Option.map (fun h -> h.Header.length) t.f

  let reset t =
    B.write t.b 0L [t.empty_header]

  let stream t =
    match t.f with
    | None -> None
    | Some header ->
      let off = ref 0
      and file_crc = ref Checkseum.Crc32.default in
      let reader buf =
        if !off = header.length then
          if Checkseum.Crc32.equal !file_crc header.file_crc then
            Lwt_result.return 0
          else
            Lwt_result.fail `Bad_checksum
        else
          let sector_size = t.info.Mirage_block.sector_size in
          let slack = !off mod sector_size in
          let want_to_read = min (Bytes.length buf) (header.length - !off) in
          let sectors = (want_to_read + pred sector_size) / sector_size in
          let cs = Cstruct.create (sectors * sector_size) in
          let b_off = 1 + !off / sector_size in
          off := !off + want_to_read;
          let* r = B.read t.b (Int64.of_int b_off) [cs] in
          match r with
          | Error e -> Lwt_result.fail (`Block e)
          | Ok () ->
            Cstruct.blit_to_bytes cs slack buf 0 want_to_read;
            file_crc :=
              Checkseum.Crc32.unsafe_digest_bytes buf 0 want_to_read !file_crc;
            Lwt_result.return want_to_read
      in
      Some reader

  let connect b =
    let* info = B.get_info b in
    if info.Mirage_block.sector_size < Header.length
    then raise (Invalid_argument "Block size too small");
    let buf = Cstruct.create info.sector_size in
    let* r = B.read b 0L [buf] in
    let () =
      match r with
      | Ok () -> ()
      | Error e -> Format.kasprintf failwith "OneFFS.connect: %a" B.pp_error e
    in
    match Header.unmarshal buf with
    | Error msg ->
      Printf.ksprintf Lwt.fail_with "bad header: %s" msg
    | Ok f ->
      (* Reuse the buffer for the empty header *)
      Cstruct.memset buf 0;
      Cstruct.blit_from_string Header.empty 0 buf 0 (String.length Header.empty);
      Lwt.return { b; info; f; empty_header = buf; }
end
