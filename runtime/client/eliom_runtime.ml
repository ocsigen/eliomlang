
(** {Misc} *)

let pos_to_string ((start,stop) : Eliom_serial.pos) =
  let open Lexing in
  let start_col = start.pos_cnum - start.pos_bol in
  let stop_col = stop.pos_cnum - stop.pos_bol in
  if start.pos_lnum = stop.pos_lnum
  then if start_col = stop_col
    then Printf.sprintf "%s %d:%d" start.pos_fname start.pos_lnum start_col
    else Printf.sprintf "%s %d:%d-%d" start.pos_fname start.pos_lnum start_col stop_col
  else Printf.sprintf "%s %d:%d-%d:%d" start.pos_fname start.pos_lnum start_col stop.pos_lnum stop_col

module Poly = struct
  type t = Eliom_serial.poly

  let make : _ -> t = Obj.magic
  let from : t -> _ = Obj.magic

  let as_fun f x = make @@ f @@ from x
end

(** {Logging} *)

let section = Lwt_log.Section.make "eliom:client"
let log_section = section
let _ = Lwt_log.Section.set_level log_section Lwt_log.Info

(** {Runtime} *)

type +'a fragment = 'a

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

module Fragment : sig
  val unwrap : Eliom_serial.Fragment_server_repr.u -> Poly.t option
  val initialize : Eliom_serial.fragment -> unit
end = struct

  let table = new%js Js.array_empty

  let find id =
    if id = 0 then (* local client value *) None else
      Js.Optdef.to_option (Js.array_get table id)

  let unwrap d = find @@ Eliom_serial.Fragment_server_repr.instance_id d

  let initialize {Eliom_serial. closure_id; args; value = server_value} =
    let closure =
      try
        Client_closure.find ~closure_id
      with Not_found ->
        let pos =
          match Eliom_serial.Fragment_server_repr.loc server_value with
          | None -> ""
          | Some p -> Printf.sprintf "(%s)" (pos_to_string p) in
        Lwt_log_js.ign_error_f ~section
          "Client closure %s not found %s (is the module linked on the client?)"
          closure_id pos ;
        exit 1
    in
    let value = closure args in
    Eliom_unwrap.late_unwrap_value server_value value;
    (* Only register global client values *)
    let instance_id = Eliom_serial.Fragment_server_repr.instance_id server_value in
    if instance_id <> 0 then Js.array_set table instance_id value
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
    Lwt_log_js.ign_debug_f ~section "Initialize injection %d" id;
    (* BBB One should assert that injection_value doesn't contain any
       value marked for late unwrapping. How to do this efficiently? *)
    Jstable.add table
      (Js.string (compilation_unit_id ^ string_of_int id))
      value

end

let get_injection = Injection.get
let register_client_closure = Client_closure.register


module Global_data = struct

  type data = {
    mutable server : Eliom_serial.fragment array list;
    mutable client : Eliom_serial.injection array list;
  }

  let tbl : data Jstable.t = Jstable.create ()

  let apply s f =
    Js.Optdef.iter (Jstable.find tbl (Js.string s)) f

  let unwrap global_data =
    let f (s, {Eliom_serial. server ; client}) =
      Jstable.add tbl (Js.string s) {
        server = Array.to_list server ;
        client = Array.to_list client ;
      }
    in
    Array.iter f global_data

  let check () = ()

end

module Request_data = struct

  let default_request_data =
    {Eliom_serial.
      global = None;
      request = [||];
    }

  let eliom_data = ref None

  let get () =
    match !eliom_data with
    | Some data -> data
    | None ->
      let eliom_request_data =
        Js.Unsafe.get Js.Unsafe.global @@
        Js.string Eliom_serial.eliom_data_id
      in
      Js.Optdef.case (Js.def eliom_request_data)
        (fun () -> eliom_data := Some default_request_data;
          default_request_data)
        (fun var ->
           let data = Eliom_unwrap.unwrap_js var in
           eliom_data := Some data;
           data)

  let _reset () =
    eliom_data := None

  let _set v =
    eliom_data := Some v

  let load request_data =
    Lwt_log_js.ign_debug_f ~section "Load request data (%a)"
      (fun () l -> string_of_int (Array.length l)) request_data;
    (* On a request, i.e. after running the toplevel definitions, global_data
       must contain at most empty sections_data lists, which stem from server-
       only eliom files. *)
    Global_data.check ();
    Array.iter Fragment.initialize request_data

end

let () =
  Eliom_unwrap.register_unwrapper'
    (Eliom_unwrap.id_of_int Eliom_serial.Fragment_server_repr.unwrap_id)
    Fragment.unwrap;
  ()


let init () =
  let js_data = Request_data.get () in

  let onload _ _ =
    begin match js_data.global with
      | Some x -> Global_data.unwrap x
      | None -> ()
    end ;
    Request_data.load js_data.request ;
    false
  in

  ignore @@ Dom_events.(listen ~capture:true Dom_html.window Typ.load onload) ;

  ()


let close_server_section compilation_unit_id =
  Lwt_log_js.ign_debug_f ~section
    "Do next client value data section in compilation unit %s"
    compilation_unit_id;
  Global_data.apply compilation_unit_id @@ fun data ->
  match data.server with
    l :: r ->
    data.server <- r;
    Array.iter Fragment.initialize l
  | [] ->
    Lwt_log_js.ign_error_f ~section
      "Queue of client value data for compilation unit %s is empty \
       (is it linked on the server?)"
      compilation_unit_id ;
    exit 1

let open_client_section compilation_unit_id =
  Lwt_log_js.ign_debug_f ~section
    "Do next injection data section in compilation unit %s"
    compilation_unit_id;
  Global_data.apply compilation_unit_id @@ fun data ->
  match data.client with
    l :: r ->
    data.client <- r;
    Array.iter (fun i -> Injection.initialize ~compilation_unit_id i) l
  | [] ->
    Lwt_log_js.ign_error_f ~section
      "Queue of injection data for compilation unit %s is empty \
       (is it linked on the server?)"
      compilation_unit_id ;
    exit 1
