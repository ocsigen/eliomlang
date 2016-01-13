
module Repr = struct

  type +'a t = {
    closure_id: int64;
    instance_id: int64;
  }

  let create ~closure_id ~instance_id =
    { closure_id; instance_id }
  (* let closure_id { closure_id } = closure_id *)
  (* let instance_id { instance_id } = instance_id *)
end

type +'a t = 'a Repr.t * Eliom_wrap.unwrapper

let unwrap_id = 7
let unwrapper = Eliom_wrap.(create_unwrapper @@ id_of_int unwrap_id)

let from_repr cv = (cv, unwrapper)
let create ~closure_id ~instance_id =
  from_repr @@ Repr.create ~closure_id ~instance_id

exception Creation_invalid_context of int64
