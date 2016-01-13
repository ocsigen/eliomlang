
type pos = Lexing.position * Lexing.position

val pos :
  string ->
  int * int * int ->
  int * int * int ->
  pos

module Poly : sig
  type t
  val make : _ -> t
  val marshall : t -> string
end

val set_global : bool -> unit



type +'a fragment = 'a Eliom_fragment.t

val fragment : pos:pos -> int64 -> 'a -> unit

val close_server_section : string -> unit

val close_client_section :
  string ->
  Poly.t list ->
  unit
