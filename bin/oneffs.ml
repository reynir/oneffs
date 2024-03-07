open Lwt.Syntax

module FS = OneFFS.Make(Block)

let connect block_size file =
  let* b = Block.connect ~prefered_sector_size:(Some block_size) file in
  FS.connect b

let get block_size file =
  let* b = connect block_size file in
  let* r = FS.read b in
  match r with
  | Ok Some data ->
    (* XXX: print newline or not? *)
    print_string data;
    Lwt.return_unit
  | Ok None ->
    prerr_endline "No data.";
    exit 1
  | Error e ->
    FS.pp_error Format.err_formatter e;
    exit 2

let stream block_size file =
  let* fs = connect block_size file in
  match FS.stream fs with
  | None ->
    prerr_endline "No data.";
    exit 1
  | Some reader ->
    let buf = Bytes.create (max 8196 block_size) in
    let rec loop () =
      let* r = reader buf in
      match r with
      | Ok 0 -> exit 0
      | Ok len ->
        output stdout buf 0 len;
        loop ()
      | Error e ->
        FS.pp_error Format.err_formatter e;
        exit 2
    in
    loop ()

let set block_size file =
  let* b = connect block_size file in
  let r = Buffer.create 4096 in
  let buf = Bytes.create 4096 in
  let rec loop () =
    let len = input stdin buf 0 4096 in
    if len = 0 then
      Buffer.contents r
    else begin
      Buffer.add_subbytes r buf 0 len;
      loop ()
    end
  in
  let* r = FS.write b (loop ()) in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e ->
    FS.pp_write_error Format.err_formatter e;
    exit 2

let get_info block_size file =
  let+ b = connect block_size file in
  match FS.size b with
  | Some size ->
    Printf.printf "Set and contains %d bytes of data.\n" size
  | None ->
    print_endline "Unset."

open Cmdliner

let block_size =
  let doc = "Block size of filesystem." in
  Arg.(value & opt int 512 & info ["b"; "block-size"] ~docv:"BLOCKSIZE" ~doc)

let oneffs_file =
  let doc = "File with OneFFS filesystem." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"ONEFFS" ~doc)

let get_cmd =
  let doc = "Get contents, if any, of OneFFS." in
  let man = [
    `S Manpage.s_description;
    `P "Get the contents of a OneFFS filesystem. If the filesystem is unset a
    message is printed on stderr and the exit status is 1.";
  ] in
  let info = Cmd.info "get" ~doc ~man in
  Cmd.v info Term.(const Lwt_main.run $ (const get $ block_size $ oneffs_file))

let stream_cmd =
  let doc = "Get contents, if any, of OneFFS - in a streaming manner." in
  let man = [
    `S Manpage.s_description;
    `P "Get the contents of a OneFFS filesystem in a streaming manner. If the
    filesystem is unset a message is printed on stderr and the exit status is
    1. If the checksum isn't valid the exit status is 2. Note that the contents
    are printed even if the checksum is invalid.";
  ] in
  let info = Cmd.info "stream" ~doc ~man in
  Cmd.v info Term.(const Lwt_main.run $ (const stream $ block_size $ oneffs_file))

let set_cmd =
  let doc = "Set contents of OneFFS." in
  let info = Cmd.info "set" ~doc in
  Cmd.v info Term.(const Lwt_main.run $ (const set $ block_size $ oneffs_file))

let info_cmd =
  let doc = "Get information of OneFFS." in
  let info = Cmd.info "info" ~doc in
  Cmd.v info Term.(const Lwt_main.run $ (const get_info $ block_size $ oneffs_file))

let main_cmd =
  let doc = "OneFFS tool" in
  let info = Cmd.info "oneffs" ~version:"%%VERSION%%" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [get_cmd; stream_cmd; set_cmd; info_cmd]

let () =
  exit (Cmd.eval main_cmd)
