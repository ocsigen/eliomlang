
let pos pos_fname (lnum1, bol1, cnum1) (lnum2, bol2, cnum2) =
  Lexing.(
    { pos_fname ;
      pos_lnum = lnum1 ;
      pos_bol = bol1 ;
      pos_cnum = cnum1 ;
    },
    { pos_fname;
      pos_lnum = lnum2 ;
      pos_bol = bol2 ;
      pos_cnum = cnum2 ;
    })

let string_escape s =
  let l = String.length s in
  let b = Buffer.create (16 * 1024) in
  let conv = "0123456789abcdef" in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
      '\000' when i = l - 1 || s.[i + 1] < '0' || s.[i + 1] > '9' ->
        Buffer.add_string b "\\0"
    | '\b' ->
        Buffer.add_string b "\\b"
    | '\t' ->
        Buffer.add_string b "\\t"
    | '\n' ->
        Buffer.add_string b "\\n"
    (*| '\011' -> (* IE<9 doesn't like vertical tab \v *)
        Buffer.add_string b "\\v"*)
    | '\012' ->
        Buffer.add_string b "\\f"
    | '\r' ->
        Buffer.add_string b "\\r"
    | '\'' ->
        Buffer.add_string b "\\'"
    | '\\' ->
        Buffer.add_string b "\\\\"
    | '\000' .. '\031' | '\127' .. '\255' | '&' | '<' | '>' ->
        let c = Char.code c in
        Buffer.add_string b "\\x";
        Buffer.add_char b conv.[c lsr 4];
        Buffer.add_char b conv.[c land 0xf]
    | _ ->
        Buffer.add_char b c
  done;
  Buffer.contents b

module Poly = struct

  type t = Eliom_serial.poly
  let make x : t = Obj.magic x

  let marshall (poly : t) =
    string_escape (Marshal.to_string (Eliom_wrap.wrap poly) [])
end



type 'a fragment = 'a Eliom_fragment.t

module StringTbl = Hashtbl.Make(struct
    include String
    let hash = Hashtbl.hash
  end)

module Global_data = struct

  type data = {
    mutable server : Eliom_serial.fragment array list;
    mutable client : Eliom_serial.injection array list;
  }

  let tbl = StringTbl.create 17

  let get id =
    if StringTbl.mem tbl id then
      StringTbl.find tbl id
    else
      let data = {
        server = [];
        client = [];
      } in
      StringTbl.add tbl id data ;
      data

  let add_client d x =
    d.client <- x :: d.client

  let add_server d x =
    d.server <- x :: d.server


  let to_serial_data ~debug {client ; server} =
    let clear_server_loc x =
      (if not debug then
         let open Eliom_serial in
         let f (x : fragment) = Fragment_server_repr.clear_loc x.value in
         Array.iter (Array.iter f) x) ;
      x
    in
    let clear_client_loc x =
      (if not debug then
         let open Eliom_serial in
         let f x = x.dbg <- None in
         Array.iter (Array.iter f) x) ;
      x
    in
    {Eliom_serial.
      server = clear_server_loc @@ Array.of_list (List.rev server) ;
      client = clear_client_loc @@ Array.of_list (List.rev client) ;
    }

  let serial ~debug : Eliom_serial.global_data =
    let r = ref 0 in
    let empty_data = {Eliom_serial. server = [||] ; client = [||] } in
    let a = Array.make (StringTbl.length tbl) ("", empty_data) in
    let f k v = a.(!r) <- (k, to_serial_data ~debug v) ; incr r in
    StringTbl.iter f tbl ;
    a



end

module Request_data = struct

  type t = Eliom_serial.fragment list

  exception Hook_alread_set
  exception Hook_not_set

  type hook = {
    get : unit -> t ;
    add : Eliom_serial.fragment -> bool ;
  }
  let hook : hook option ref = ref None

  let set_functions get add =
    match !hook with
    | None -> hook := Some { get ; add }
    | Some _ -> raise Hook_alread_set

  let get_functions () =
    match !hook with
    | None -> raise Hook_not_set
    | Some f -> f

  let get () = (get_functions()).get ()
  let add x = (get_functions()).add x

  let serial ~debug : Eliom_serial.request_data =
    let clear_client_loc x =
      (if not debug then
         let open Eliom_serial in
         let f (x : fragment) = Fragment_server_repr.clear_loc x.value in
         Array.iter f x) ;
      x
    in
    clear_client_loc @@ Array.of_list @@ List.rev @@ get ()

end


let closing_cdata = Re.(compile @@ str "]]>")
let escape_cdata_script s =
  (* For security reasons, we do not allow "]]>" inside CDATA
     (as this string is to be considered as the end of the cdata)
  *)
  Printf.sprintf
    {|\n//<![CDATA[\n%s\n//]]>\n|}
    (Re.replace_string ~all:true closing_cdata ~by:"" s)

let eliom_script global request  =
  let global = Some global in
  let data = {Eliom_serial. global ; request } in
  let script =
    Printf.sprintf
      "%s = \'%s\';"
      Eliom_serial.eliom_data_id
      (string_escape (Marshal.to_string data []))
  in
  escape_cdata_script script

let current_server_section_data = ref []

let close_server_section compilation_unit_id =
  let data = Global_data.get compilation_unit_id in
  Global_data.add_server data @@
  Array.of_list @@ List.rev !current_server_section_data;
  current_server_section_data := []

let close_client_section compilation_unit_id injection_data =
  let data = Global_data.get compilation_unit_id in
  let injection_datum (id, value, loc) =
    {Eliom_serial. id; value ; dbg = Some loc }
  in
  let injection_data = Array.of_list injection_data in
  Global_data.add_client data @@
  Array.map injection_datum injection_data


let is_global = ref false
let set_global b = is_global := b

let register_fragment ~closure_id ~args ~value =
  let fragment_datum =
    Eliom_fragment.serial ~closure_id ~args value
  in
  let b = Request_data.add fragment_datum in
  if b then
    (* Succesfully added to request data, nothing more to do *)
    ()
  else
    (* Not in a request, must add to global data . *)
    current_server_section_data :=
      fragment_datum :: !current_server_section_data

let last_id = ref 0

let fragment ?pos closure_id args =
  let id =
    if !is_global then begin
      incr last_id;
      !last_id
    end else
      0
  in
  let args = Poly.make args in
  let value = Eliom_fragment.create ?loc:pos ~id in
  register_fragment ~closure_id ~args ~value;
  value
