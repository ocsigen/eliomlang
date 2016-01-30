
type pos = Lexing.position * Lexing.position

type poly

module Fragment_server_repr = struct

  type u = {
    mutable loc : pos option;
    id: int;
    unwrapper: Eliom_wrap.unwrapper
  }
  type 'a t = u

  let create ?loc ~id ~unwrapper = { id; loc; unwrapper }
  let instance_id cv = cv.id
  let loc cv = cv.loc
  let clear_loc cv = cv.loc <- None
  let to_poly v = v
end

type fragment = {
  closure_id : string;
  args : poly;
  value : poly Fragment_server_repr.t
}

type injection = {
  dbg : (pos * string option) option;
  id : int;
  value : poly;
}

type compilation_unit_global_data = {
  server : fragment array array;
  client : injection array array;
}

type global_data = (string * compilation_unit_global_data) array

type request_data = fragment array
