module Make(B : Mirage_block.S) : sig
  type t

  type error = [ `Block of B.error | `Bad_checksum ]

  type write_error = B.write_error

  val pp_error : error Fmt.t

  val pp_write_error : write_error Fmt.t

  val write : t -> string -> (unit, write_error) result Lwt.t
  (** [write fs data] stores [data]. An error is returned if writing to the
      underlying block device fails. *)

  val read : t -> (string option, error) result Lwt.t
  (** [read fs] reads the data stored if any. An error is returned if the
      checksum is bad or if the read fails. *)

  val is_set : t -> bool
  (**  [is_set fs] is true if [fs] has any data. *)

  val size : t -> int option
  (** [size fs] is [Some size] if the contents is [size] bytes long, or [None] if unset. *)

  val reset : t -> (unit, write_error) result Lwt.t
  (** [reset fs] sets [fs] to no data. *)

  val format : B.t -> (unit, write_error) result Lwt.t
  (** [format b] writes an empty header at the beginning of [b]. Note that this
      is destructive. *)

  val stream : t -> (bytes -> (int , error) result Lwt.t) option
(** [stream fs] is [Some reader] where [reader buf] is [Ok bytes_read] and
    fills [buf] with [bytes_read] bytes read from the file.
    [reader] can be called repeatedly to read subsequent bytes from the file in
    a streaming manner. When the end of file is reached then either [Ok 0] is
    returned or [Error `Bad_checksum] if the file contents do not match with
    the checksum from the header when [stream fs] was called.
    NOTE: the caller can only know at the end of the file if the data read is
    corrupt or not, and should try to recover in that case. *)

  val connect : B.t -> t Lwt.t
end
