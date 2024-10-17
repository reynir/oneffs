module Make(_ : Mirage_clock.PCLOCK)(Block : Mirage_block.S) : sig
  type t

  val connect : Block.t -> (t * string option) Lwt.t
  (** [connect block] is [(fs, data_opt)] where [data_opt] is [Some data] if
      [block] contains data, or [None] if either it didn't contain data or the
      checksum was invalid, and in both cases the disk will be initialized.

      @raise Failure if reading or initializing fails. *)

  val write : t -> string -> (unit, [> `Msg of string ]) result Lwt.t
  (** [write fs data] stores [data]. An error is returned if the write fails. *)

  val read : t -> (string, [> `Msg of string ]) result Lwt.t
  (** [read fs] reads the stored. An error is returned if the read fails, if
      the checksum is bad or if no data is stored. *)
end
