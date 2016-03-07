open Typedtree

val get_section_side :
  structure_item_desc ->
  structure_item_desc * [> `Client | `Server | `Shared ] option

type eliom_expr_content = {
  attrs : attributes ;
  id : string ;
  expr : expression
}

type eliom_expr =
  | Expr of expression
  | Fragment of eliom_expr_content
  | Injection of eliom_expr_content

val unfold_expression : expression -> eliom_expr

module Collect : sig
  val escaped : expression -> eliom_expr_content list
  val injections : structure_item -> eliom_expr_content list
end

module CollectMap : sig
  val escaped :
    (eliom_expr_content -> Parsetree.expression) ->
    expression -> Parsetree.expression
  val injections :
    (eliom_expr_content -> Parsetree.expression) ->
    structure_item -> Parsetree.structure_item
end

module Name : sig

  val make_injection_id : loc:Location.t -> string -> Parsetree.expression

  val annotate : Typedtree.structure -> Typedtree.structure

end
