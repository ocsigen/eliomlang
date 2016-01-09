
let wrap_and_marshall_poly : poly -> string =
  fun poly ->
    string_escape (Marshal.to_string (Eliom_wrap.wrap poly) [])

type +'a client_value = 'a Client_value_server_repr.t * Eliom_wrap.unwrapper

let client_value_unwrapper =
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_lib_base.client_value_unwrap_id_int)

let create_client_value cv = (cv, client_value_unwrapper)
