
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

module Global_data : sig

  val serial : debug:bool -> Eliom_serial.global_data

end

module Request_data : sig

  type t = Eliom_serial.fragment list

  exception Hook_alread_set
  exception Hook_not_set

  val set_functions :
    (unit -> t) ->
    (Eliom_serial.fragment -> bool) ->
    (unit -> bool) ->
    unit

  val serial : debug:bool -> Eliom_serial.request_data

end

val eliom_script :
  Eliom_serial.global_data -> Eliom_serial.request_data -> string

type +'a[@client] fragment = 'a Eliom_fragment.t

(** Registers a client fragment datum for the next server section when
    executed in a global_data
    (cf. {!Eliom_runtime.set_global}) or in the
    request_data when executed in a request. *)
val fragment : ?pos:Eliom_serial.pos -> string -> 'args -> 'a fragment

(** Called at the end of each server or shared section. The argument
    identifies the compilation unit.

    Adds the list of recently registered
    {!Eliom_lib_base.client_value_datum}s into the queue of server
    section data of the compilation unit
    ({!Eliom_lib_base.compilation_unit_global_data}).

    Called in parallel with <<a_api
    subproject="client"|Eliom_client.Syntax_helpers.close_server_section>>.  *)
val close_server_section : string -> unit

(** Called at the end of every client or shared section. The first
    argument identifies the compilation unit. The second is the list
    of novel injections in that section.

    Adds a list of {!Eliom_lib_base.injection_datum}s into the queue
    of client section data of the compilation unit
    ({!Eliom_lib_base.compilation_unit_global_data}).

    Called in parallel with <<a_api
    subproject="client"|Eliom_client.Syntax_helpers.open_client_section>>.  *)
val close_client_section :
  string ->
  (int * Poly.t * Eliom_serial.pos) list ->
  unit
