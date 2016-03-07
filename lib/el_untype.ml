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

let string_of_section = function
  | "eliom.client" -> Some `Client
  | "eliom.server" -> Some `Server
  | "eliom.shared" -> Some `Shared
  | _ -> None

(* C style return type, so elegant! *)
let rec remove_section_attr r = function
  | [] -> []
  | h :: t ->
    match string_of_section (fst h).Location.txt with
    | None -> h :: remove_section_attr r t
    | Some _ as x -> r := x ; t

let get_section_side str =
  let r = ref None in
  let str' = map_mod_attr (remove_section_attr r) str in
  str', !r

(** Utilities on fragments and injections *)

type eliom_expr =
  | Expr of expression
  | Fragment of
      { attrs : attributes ; id : string ; expr : expression }
  | Injection of
      { attrs : attributes ; id : string ; expr : expression }

let unfold_expression expr =
  let rec aux acc = function
    | [] -> Expr {expr with exp_attributes = List.rev acc}
    | ({Location. txt = "eliom.fragment"},_)::t ->
      Fragment {attrs = List.rev acc ; id = "foo" ; expr = {expr with exp_attributes = t} }
    | ({Location. txt = "eliom.injection"},_)::t ->
      Injection {attrs = List.rev acc ; id = "foo" ; expr = {expr with exp_attributes = t} }
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
          | Injection {id ; attrs ; expr} -> r := (id, attrs , expr) :: !r
          | Fragment _ -> assert false
      end)
  end

  let escaped e =
    let module M = Make () in
    M.iter_expression e ;
    List.rev !M.r

  let injections stri =
    let module M = Make () in
    M.iter_structure_item stri ;
    List.rev !M.r

end

module CollectMap = struct

  let make f =
    let expr mapper expr = match unfold_expression expr with
      | Injection {id;attrs;expr} ->
        f id expr attrs
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
