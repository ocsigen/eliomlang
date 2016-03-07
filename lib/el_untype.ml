open Ast_helper
open Typedtree
module U = Untypeast

(** Utilities on sections. *)

let map_mod_attr f = function
  | Tstr_eval (e, attrs) -> Tstr_eval (e, f attrs)
  | Tstr_primitive x ->
      Tstr_primitive {x with val_attributes = f x.val_attributes}
  | Tstr_value (rc,x) ->
      Tstr_value (rc,List.map
          (fun x -> {x with vb_attributes = f x.vb_attributes}) x)
  | Tstr_type (r,l) ->
      Tstr_type (r,List.map
          (fun x -> {x with typ_attributes = f x.typ_attributes}) l)
  | Tstr_typext tex ->
      Tstr_typext {tex with tyext_attributes = f tex.tyext_attributes}
  | Tstr_exception exn ->
      Tstr_exception {exn with ext_attributes = f exn.ext_attributes}
  | Tstr_module mb ->
      Tstr_module {mb with mb_attributes = f mb.mb_attributes}
  | Tstr_recmodule rmb ->
      Tstr_recmodule (List.map
          (fun mb -> {mb with mb_attributes = f mb.mb_attributes}) rmb)
  | Tstr_modtype mt ->
      Tstr_modtype {mt with mtd_attributes = f mt.mtd_attributes}
  | Tstr_open op ->
      Tstr_open {op with open_attributes = f op.open_attributes}
  | Tstr_include ic ->
      Tstr_include {ic with incl_attributes = f ic.incl_attributes}
  | Tstr_class cls ->
      Tstr_class (List.map
          (fun (cl,s) -> {cl with ci_attributes = f cl.ci_attributes}, s) cls)
  | Tstr_class_type clt ->
      Tstr_class_type (List.map
          (fun (id,s,cl) -> id,s,{cl with ci_attributes = f cl.ci_attributes})
            clt)
  | Tstr_attribute at ->
      Tstr_attribute at

(** Attribute extraction *)
module Attr = struct
  let get {Location.txt} = match txt with
    | "eliom.fragment"  -> Some `Fragment
    | "eliom.injection" -> Some `Injection
    | _ -> None

  let section {Location.txt} = match txt with
    | "eliom.client" -> Some `Client
    | "eliom.server" -> Some `Server
    | "eliom.shared" -> Some `Shared
    | _ -> None

  let rec map f : attributes -> attributes = function
    | [] -> []
    | (id, payload) as h :: t -> match get id with
      | Some x -> (id, f x payload) :: map f t
      | None -> h :: map f t

end



(* C style return type, so elegant! *)
let rec remove_section_attr r = function
  | [] -> []
  | h :: t ->
    match Attr.section (fst h) with
    | None -> h :: remove_section_attr r t
    | Some _ as x -> r := x ; t

let get_section_side str =
  let r = ref None in
  let str' = map_mod_attr (remove_section_attr r) str in
  str', !r


(** Utilities on fragments and injections *)

type eliom_expr_content = {
  attrs : attributes ;
  id : string ;
  expr : expression
}

type eliom_expr =
  | Expr of expression
  | Fragment of eliom_expr_content
  | Injection of eliom_expr_content

let unfold_expression expr =
  let build attrs expr payload =
    let open Parsetree in match payload with
    | PStr [{pstr_desc=Pstr_eval
                   ({pexp_desc=Pexp_constant Pconst_string (id,_)},[])}] ->
      { attrs ; id ; expr }
    | _ -> Location.raise_errorf ~loc:expr.exp_loc
        "Eliom ICE: Malformed identifier."
  in
  let rec aux acc = function
    | [] -> Expr {expr with exp_attributes = List.rev acc}
    | (a, payload)::t when Attr.get a = Some `Fragment ->
      Fragment (build (List.rev acc) {expr with exp_attributes = t} payload)
    | (a, payload)::t when Attr.get a = Some `Injection ->
      Injection (build (List.rev acc) {expr with exp_attributes = t} payload)
    | h :: t -> aux (h::acc) t
  in aux [] expr.exp_attributes

module Collect = struct

  module Make () = struct
    let r = ref []

    include TypedtreeIter.MakeIterator(struct
        include TypedtreeIter.DefaultIteratorArgument
        let enter_expression e =
          match unfold_expression e with
          | Expr _ -> ()
          | Injection x -> r := x :: !r
          | Fragment _ -> assert false
      end)
  end

  let escaped e =
    let module M = Make () in
    M.iter_expression e ;
    !M.r

  let injections stri =
    let module M = Make () in
    M.iter_structure_item stri ;
    !M.r

end

module CollectMap = struct

  let make f =
    let expr mapper expr = match unfold_expression expr with
      | Injection x -> f x
      | _ ->
        U.default_mapper.expr mapper expr
    in
    {U.default_mapper with expr}

  let escaped f e =
    let m = make f in
    m.expr m e

  let injections f e =
    let m = make f in
    m.structure_item m e
end

(** Identifiers generation. *)
(* This is a global pass on the complete typedtree. It stores each injections,
   escaped values and fragments in a map with a new generated identifier.
   Fragments and injections ids are global to the file (plus the file hash).
   Escaped values are local to the fragment.

   The comparison function is tuned to share identical identifiers. It doesn't
   share identical expressions (due to potential side effects).
*)
module Name = struct
  module M = Map.Make(struct
      type t = expression
      let compare x y = match x.exp_desc ,y.exp_desc with
        | Texp_ident (p1,_,_), Texp_ident (p2,_,_) -> compare p1 p2
        | _ -> compare x y (* Not recursive! *)
    end )

  module Map = struct
    type t = { i : int ; map : (int * string) M.t }
    let empty = { i = 0 ; map = M.empty }

    let add make expr {i; map} =
      if M.mem expr map
      then snd @@ M.find expr map, {i ; map}
      else
        let hash = El_utils.file_hash expr.exp_loc in
        let s = make hash i in
        let v = (i,s) in
        let i = i + 1 in
        s, {i ; map = M.add expr v map }
  end

  let escaped_ident_fmt : _ format6 =
    "_eliom_escaped_%d"

  let hash_fmt : _ format6 =
    "%s%s"

  let add_escaped =
    let make _ i = Printf.sprintf escaped_ident_fmt i in
    Map.add make

  let add_injection =
    let make _hash i = string_of_int i in
    Map.add make

  let add_fragment =
    let make hash i = Printf.sprintf "%s%d" hash i in
    Map.add make

  let make_injection_id ~loc txt =
    let hash = El_utils.file_hash loc in
    Exp.constant ~loc @@ Const.string (Printf.sprintf hash_fmt hash txt)

  (* This implementation is complicated by the fact that typedtree iterators
     are not functional. We need to maintain references and stack manually.
  *)
  (* This is probably buggy for multi nested frag/inj. TODO *)
  let annotate str =
    let fragmap = ref Map.empty in
    let injmap = ref Map.empty in
    let escmaps = Stack.create () in
    let add_frag frag =
      Stack.push Map.empty escmaps ;
      let s, m = add_fragment frag !fragmap in fragmap := m ; s
    in
    let add_inj inj =
      if Stack.is_empty escmaps then
        let s, m = add_injection inj !injmap in injmap := m ; s
      else
        let s, m = add_escaped inj @@ Stack.pop escmaps in
        Stack.push m escmaps ; s
    in
    let annotate e x _ = match x with
      | `Injection ->
        Parsetree.PStr [Str.eval (Exp.constant (Const.string @@ add_inj e))]
      | `Fragment ->
        Parsetree.PStr [Str.eval (Exp.constant (Const.string @@ add_frag e))]
    in

    let module M = TypedtreeMap.MakeMap(struct
        include TypedtreeMap.DefaultMapArgument

      let enter_expression e =
        {e with exp_attributes = Attr.map (annotate e) e.exp_attributes}

      let leave_expression e =
        let f x p = match x with
          | `Fragment -> ignore @@ Stack.pop escmaps ; p
          | `Injection -> p
        in
        ignore @@ Attr.map f e.exp_attributes ; e
      end)
    in
    M.map_structure str

end
