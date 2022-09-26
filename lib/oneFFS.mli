module Make(B : Mirage_block.S) : sig
  type t

  type error = [ `Block of B.error | `Bad_checksum ]

  type write_error = B.write_error

  val pp_error : error Fmt.t

  val pp_write_error : write_error Fmt.t

  val write : t -> string -> (unit, write_error) result Lwt.t

  val read : t -> (string option, error) result Lwt.t

  val is_set : t -> bool

  val connect : B.t -> t Lwt.t
end
