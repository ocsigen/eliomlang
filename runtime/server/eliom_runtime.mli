
val pos :
  string ->
  int * int * int ->
  int * int * int ->
  Eliom_serial.pos

module Poly : sig
  type t
  val make : _ -> t
  val marshall : t -> string
end

val set_global : bool -> unit

module Global_data : sig

  val serial : unit -> Eliom_serial.global_data

end

module Request_data : sig

  type t = Eliom_serial.fragment list

  exception Hook_alread_set
  exception Hook_not_set

  val set_functions : (unit -> t) -> (Eliom_serial.fragment -> unit) -> unit

  val serial : unit -> Eliom_serial.request_data

end


type +'a fragment = 'a Eliom_fragment.t

val fragment : ?pos:Eliom_serial.pos -> string -> 'args -> 'a fragment

val close_server_section : string -> unit

val close_client_section :
  string ->
  (int * Poly.t * Eliom_serial.pos * string option) list ->
  unit
