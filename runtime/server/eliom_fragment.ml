module Repr = Eliom_serial.Fragment_server_repr

type +'a[@client] t = 'a Repr.t

let unwrapper = Eliom_wrap.(create_unwrapper @@ id_of_int Repr.unwrap_id)

let create ?loc ~id =
  Repr.create ?loc ~id ~unwrapper

let serial ~closure_id ~args value =
  {Eliom_serial.closure_id; args; value = Repr.to_poly value}

exception Creation_invalid_context of int64
