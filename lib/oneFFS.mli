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

  val reset : t -> (unit, write_error) result Lwt.t
  (** [reset fs] sets [fs] to no data. *)

  val format : B.t -> (unit, write_error) result Lwt.t
  (** [format b] writes an empty header at the beginning of [b]. Note that this
      is destructive. *)

  val connect : B.t -> t Lwt.t

  module RO : sig
    include Mirage_kv.RO with type t = t
    val size : t -> key -> (int, error) result Lwt.t
  end

  module RW : sig
    include Mirage_kv.RW with type t = t
    val rename : t -> source:key -> dest:key -> (unit, write_error) result Lwt.t
  end
end
