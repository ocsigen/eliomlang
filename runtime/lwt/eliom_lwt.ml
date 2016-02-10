open Lwt.Infix


let hook = Lwt.new_key ()

let flush () = match Lwt.get hook with
  | Some x ->
    let v = !x in x := [] ; v
  | None ->
    (* "flush" is only called by serial, which should only be called at the
       end of a request handling. Hence this should never happen. *)
    assert false

let add frag =
  match Lwt.get hook with
  | Some l -> l := frag :: !l ; true
  | None -> false

let () = Eliom_runtime.Request_data.set_functions flush add

let handle_request ~debug f =
  let f () =
    f () >>= fun v ->
    let req_data = Eliom_runtime.Request_data.serial ~debug in
    Lwt.return (req_data, v)
  in
  Lwt.with_value hook (Some (ref [])) f

let detach_from_request f =
  Lwt.with_value hook None f
