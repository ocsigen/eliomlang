
type +'a t

val create : ?loc:Eliom_serial.pos -> id:int -> _ t

val serial : closure_id:string -> args:Eliom_serial.poly -> _ t -> Eliom_serial.fragment

exception Creation_invalid_context of int64
