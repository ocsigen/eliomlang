open Parsetree
open Ast_helper

module AM = Ast_mapper
module AC = Ast_convenience

(** Various misc functions *)

let flatmap f l = List.flatten @@ List.map f l

let rec fold_accum f l acc = match l with
  | [] -> []
  | h :: t ->
    let acc, newl = f acc h in
    newl @ fold_accum f t acc

let get_extension = function
  | {pexp_desc= Pexp_extension ({txt},_)} -> txt
  | _ -> invalid_arg "Eliom ppx: Should be an extension."

let (%) f g x = f (g x)

let exp_add_attrs attr e =
  {e with pexp_attributes = attr}

let id_of_string str =
  Printf.sprintf "%019d" (Hashtbl.hash str)

let file_loc () =
  Location.in_file !Location.input_name

let eid {Location. txt ; loc } =
  Exp.ident ~loc { loc ; txt = Longident.Lident txt }

let error f ?sub ?loc =
  Format.ksprintf (fun s -> f ?loc ?attrs:None @@ AM.extension_of_error @@ Location.error ?loc ?sub s)

let exp_error ?sub ~loc = error Exp.extension ?sub ~loc
let str_error ?sub ~loc = error Str.extension ?sub ~loc
let sig_error ?sub ~loc = error Sig.extension ?sub ~loc

let format_args = function
  | [] -> AC.unit ()
  | [e] -> e
  | l -> Exp.tuple l

let pat_args = function
  | [] -> AC.punit ()
  | [p] -> p
  | l -> Pat.tuple l

let file_hash loc =
  Hashtbl.hash @@ loc.Location.loc_start.pos_fname

let lexing_position ~loc l =
  [%expr
    { Lexing.pos_fname = [%e AC.str l.Lexing.pos_fname];
      Lexing.pos_lnum = [%e AC.int @@ l.Lexing.pos_lnum];
      Lexing.pos_bol = [%e AC.int @@ l.Lexing.pos_bol];
      Lexing.pos_cnum = [%e AC.int @@ l.Lexing.pos_cnum]; }
  ] [@metaloc loc]

let position loc =
  let start = loc.Location.loc_start in
  let stop = loc.Location.loc_start in
  Exp.tuple ~loc [ lexing_position ~loc start ; lexing_position ~loc stop ]

let is_annotation txt l =
  List.exists (fun s -> txt = s || txt = "eliom."^s) l

(** Identifiers generation. *)
module Name = struct

  module M = Map.Make(struct
      type t = expression
      let compare x y = match x.pexp_desc ,y.pexp_desc with
        | Pexp_ident {txt = s1}, Pexp_ident {txt = s2} -> compare s1 s2
        | _ -> compare x y
    end )

  module Map = struct
    type t = { i : int64 ; map : string M.t }
    let empty = { i = 0L ; map = M.empty }

    let add make expr {i; map} =
      if M.mem expr map
      then M.find expr map, {i ; map}
      else
        let hash = file_hash expr.pexp_loc in
        let s = make hash i in
        let i = Int64.(add one) i in
        s, {i ; map = M.add expr s map }

    let bindings {map} = M.bindings map

  end

  let escaped_ident_fmt : _ format6 =
    "_eliom_escaped_ident_%Ld"

  let fragment_ident_fmt : _ format6 =
    "_eliom_fragment_%Ld"

  let injected_ident_fmt : _ format6 =
    "_eliom_injected_ident_%019d_%Ld"

  let add_escaped_value =
    let make _ i = Printf.sprintf escaped_ident_fmt i in
    Map.add make

  let add_injection =
    let make hash i = Printf.sprintf injected_ident_fmt hash i in
    Map.add make

  let add_fragment =
    let make _ i = Printf.sprintf fragment_ident_fmt i in
    Map.add make

end

(** Context convenience module. *)
module Context = struct

  let to_string = function
    | `Client -> "client"
    | `Shared -> "shared"
    | `Server -> "server"

  let of_string = function
    | "server" | "server.start" -> `Server
    | "shared" | "shared.start" -> `Shared
    | "client" | "client.start" -> `Client
    | _ -> invalid_arg "Eliom ppx: Not a context"

  type escape_inject = [
    | `Escaped_value
    | `Injection
  ]

  type t = [
    | `Server (* [%%server ... ] *)
    | `Client (* [%%client ... ] *)
    | `Fragment (* [%client ... ] *)
    | `Escaped_value (* [%shared ~%( ... ) ] *)
    | `Injection (* [%%client ~%( ... ) ] *)
  ]

  type shared = [
    | `Shared
    | t
  ]
end

let open_eliom_pervasives = [%stri open Eliom_pervasives ]

(**
   Replace shared expression by the equivalent pair.

   [ [%share
       let x = ... %s ... in
       [%client ... %x ... ]
     ] ]
   ≡
   [ let x = ... s ... in
     [%client ... %x ... ]
     ,
     [%client
       let x = ... %s ... in
       ... x ...
     ]
   ]
*)

module Shared = struct

  type 'a t = { client : 'a ; server : 'a }

  let server = object
    inherit Ppx_core.Ast_traverse.map as super
    method! expression expr = match expr with
      | [%expr [%client [%e? _ ]]] -> expr
      | [%expr ~% [%e? injection_expr ]] -> injection_expr
      | _ -> super#expression expr
  end

  let client = object
    inherit [_] Ppx_core.Ast_traverse.map_with_context as super
    method! expression ctx expr = match expr with
      | [%expr [%client [%e? fragment_expr ]]] ->
        super#expression `Fragment fragment_expr
      | [%expr ~% [%e? injection_expr ]] ->
        begin match ctx with
          | `Top -> expr
          | `Fragment -> injection_expr
        end
      | _ -> super#expression ctx expr
  end

  let expression loc expr =
    let server_expr = server#expression expr in
    let client_expr = client#expression `Top expr in
    [%expr
      Eliom_lib.create_shared_value
        [%e server_expr]
        [%client [%e client_expr]]
    ] [@metaloc loc]

  let structure_item stri =
    let server = server#structure_item stri in
    let client = client#structure_item `Top stri in
    { client ; server }

  let signature_item sigi =
    let server = server#signature_item sigi in
    let client = client#signature_item `Top sigi in
    { client ; server }

end

(** Collect all the injection expressions and substitute them by fresh
    variables. Returns a map from variable to injections.

    Can be applied both to client section and fragments.
*)
let collect_injections = object
  inherit [_] Ppx_core.Ast_traverse.fold_map as super
  method! expression expr acc = match expr with
    | [%expr ~% [%e? inj ]] ->
      let (s, m) = Name.add_injection inj acc in
      let loc = expr.pexp_loc in
      let e = Exp.ident ~loc @@ Location.mkloc (Longident.Lident s) loc in
      e, m
    | _ ->
      super#expression expr acc
end

let value_binding_of_map m =
  let f (e, s) =
    let loc = e.pexp_loc in
    Vb.mk ~loc (Pat.var ~loc @@ Location.mkloc s loc) e
  in
  List.map f @@ Name.Map.bindings m

(** Given a name map, create an expression of the form
    ((fun _ _ _ -> assert false) e_1 .. e_n)

    The resulting expression should be of type [∀ 'a. 'a].
*)
let make_poly ~loc m =
  let l = Name.Map.bindings m in
  let args = List.map (fun (e,_) -> ("", e)) l in
  let assert_false = [%expr assert false][@metaloc loc] in
  let rec aux = function
    | [] -> assert_false
    | _ :: t -> [%expr fun _ -> [%e aux t]][@metaloc loc]
  in
  Exp.apply ~loc (aux l) args

let prelim = object (self)
  inherit [Context.shared] Ppx_core.Ast_traverse.map_with_context as super

  method! expression context expr =
    let loc = expr.pexp_loc in
    let attrs = expr.pexp_attributes in
    match expr, context with
    | {pexp_desc = Pexp_extension ({txt},_)},
      `Client
      when is_annotation txt ["client"; "shared"] ->
      let side = get_extension expr in
      exp_error ~loc
        "The syntax [%%%s ...] is not allowed inside client code."
        side
    | {pexp_desc = Pexp_extension ({txt},_)}
    , (`Fragment | `Escaped_value | `Injection)
      when is_annotation txt ["client"; "shared"] ->
      let side = get_extension expr in
      exp_error ~loc
        "The syntax [%%%s ...] can not be nested."
        side

    (* [%shared ... ] *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (frag_exp,attrs')}])},
      `Server
      when is_annotation txt ["shared"] ->
      let e = Shared.expression loc frag_exp in
      self#expression context @@ exp_add_attrs (attrs@attrs') e

    (* [%client e ] with e = ... ~%x ...

       let escp1 = x in
       ((fun _ -> assert false) escp1)[@eliom.fragment a]
    *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (frag_exp,attrs)}])},
      `Server
      when is_annotation txt ["client"] ->
      let frag_exp = self#expression `Client frag_exp in
      let frag_exp, m = collect_injections#expression frag_exp Name.Map.empty in
      let poly_exp = make_poly ~loc m in
      let e = Exp.let_ ~loc Nonrecursive (value_binding_of_map m) poly_exp in
      let eliom_attr = Location.mkloc "eliom.fragment" loc, PStr [Str.eval frag_exp] in
      exp_add_attrs (eliom_attr :: attrs) e

    (* ~%( ... ) ] *)
    | [%expr ~% [%e? inj ]], _ ->
      begin match context with
        | `Client ->
          let context = `Injection in
          super#expression context inj
        | `Fragment ->
          let context = `Escaped_value in
          super#expression context inj
        | `Server ->
          exp_error ~loc "The syntax ~%% ... is not allowed inside server code."
        | `Escaped_value | `Injection ->
          exp_error ~loc "The syntax ~%% ... can not be nested."
        | `Shared ->
          assert false (* TODO *)
      end
    | _ -> super#expression context expr

  (** Client section translation.

      We collect all injections, hoist them, and hide the client code in
      a ppx attribute. See also client fragments.
  *)
  method private client_section stri =
    let loc = stri.pstr_loc in
    let stri = self#structure_item `Client stri in
    let stri, m = collect_injections#structure_item stri Name.Map.empty in
    let poly_exp = make_poly ~loc m in
    let bindings = Str.value ~loc Nonrecursive (value_binding_of_map m) in
    let eliom_attr = Location.mkloc "eliom.section" loc, PStr [stri] in
    let eliom_expr = exp_add_attrs [eliom_attr] poly_exp in
    let s = [%stri let _ = [%e eliom_expr] ][@metaloc loc] in
    [ bindings ; s ]


  (** Toplevel translation *)
  method private dispatch_str context x =
    match context with
    | `Shared ->
      let f x =
        let x = Shared.structure_item x in
        self#structure `Client [x.client] @ self#structure `Server [x.server]
      in flatmap f x
    | `Client ->
      flatmap self#client_section x
    | #Context.t as c -> List.map (self#structure_item c) x

  method private dispatch_sig context x =
    match context with
    | `Shared ->
      let f x =
        let x = Shared.signature_item x in
        self#signature `Client [x.client] @ self#signature `Server [x.server]
      in flatmap f x
    | #Context.t as c -> List.map (self#signature_item c) x

  method! structure context structs =
    let f c pstr =
      let loc = pstr.pstr_loc in
      match pstr.pstr_desc with
      | Pstr_extension (({txt}, payload), _)
        when is_annotation txt ["shared.start"; "client.start" ;"server.start"] ->
        begin match payload with
          | PStr [] -> (Context.of_string txt, [])
          | _ ->
            c, [ str_error ~loc "Wrong payload for the %%%%%s extension." txt ]
        end
      | Pstr_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
        (c, self#dispatch_str (Context.of_string txt) strs)
      | Pstr_extension (({txt}, _), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
          c, [ str_error ~loc "Wrong payload for the %%%%%s extension." txt ]
      | _ ->
        (c, self#dispatch_str c [pstr])
    in
    open_eliom_pervasives :: fold_accum f structs context

  method! signature context sigs =
    let f c psig =
      let loc = psig.psig_loc in
      match psig.psig_desc with
      | Psig_extension (({txt=("shared.start"|"client.start"|"server.start" as txt)}, PStr strs), _) ->
        if strs <> [] then
          c, [ sig_error ~loc
              "The %%%%%s extension doesn't accept arguments." txt ]
        else (Context.of_string txt, [])
      | _ ->
        (c, self#dispatch_sig c [psig])
    in
    fold_accum f sigs context

end



let mapper _args =
  let c = `Server in
  {AM.default_mapper
   with
    structure = (fun _ -> prelim#structure c) ;
    signature = (fun _ -> prelim#signature c) ;
  }
