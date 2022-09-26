module Make(B : Mirage_block.S) : sig
  type t

  type error = [ `Block of B.error | `Bad_checksum ]

  type write_error = [ `Block_write of B.write_error ]

  val write : t -> string -> (unit, write_error) result Lwt.t

  val read : t -> (string option, error) result Lwt.t

  val is_set : t -> bool

  val connect : B.t -> (t, [> `Block of B.error | `Msg of string ]) result Lwt.t
end
