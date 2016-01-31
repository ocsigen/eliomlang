
type +'a fragment = 'a

module Poly = struct
  type t = Eliom_serial.poly

  let make : _ -> t = Obj.magic
  let from : t -> _ = Obj.magic

  let as_fun f x = make @@ f @@ from x

end

module Client_closure : sig
  val register : closure_id:string -> (_ -> _) -> unit
  val find : closure_id:string -> (Poly.t -> Poly.t)
end = struct

  let client_closures : (Poly.t -> Poly.t) Jstable.t = Jstable.create ()

  let register ~closure_id closure =
    Jstable.add client_closures (Js.string closure_id) (Poly.as_fun closure)

  let find ~closure_id =
    Js.Optdef.get
      (Jstable.find client_closures (Js.string closure_id))
      (fun () -> raise Not_found)
end

module Injection : sig
  val get : ?pos:Eliom_serial.pos -> string -> _
  val initialize : compilation_unit_id:string -> Eliom_serial.injection -> unit
end = struct

  let table = Jstable.create ()

  exception Unknown of (string * Eliom_serial.pos option)

  let get ?pos name =
    Poly.from @@ Js.Optdef.get
      (Jstable.find table (Js.string name))
      (fun () -> raise (Unknown (name, pos)))

  let initialize ~compilation_unit_id
        {Eliom_serial. id; value } =
    (* Lwt_log.ign_debug_f ~section "Initialize injection %d" injection_id; *)
    (* BBB One should assert that injection_value doesn't contain any
       value marked for late unwrapping. How to do this efficiently? *)
    Jstable.add table
      (Js.string (compilation_unit_id ^ string_of_int id))
      value

end

let get_injection = Injection.get
let register_client_closure = Client_closure.register
