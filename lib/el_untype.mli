open Typedtree

val get_section_side :
  structure_item_desc ->
  structure_item_desc * [> `Client | `Server | `Shared ] option

type eliom_expr =
  | Expr of expression
  | Fragment of
      { attrs : attributes ; id : string ; expr : expression }
  | Injection of
      { attrs : attributes ; id : string ; expr : expression }

val unfold_expression : expression -> eliom_expr

module Collect : sig
  val escaped : expression -> (string * attributes * expression) list
  val injections : structure_item -> (string * attributes * expression) list
end

module CollectMap : sig
  val escaped :
    (string -> expression -> attributes -> Parsetree.expression) ->
    expression -> Parsetree.expression
  val injections :
    (string -> expression -> attributes -> Parsetree.expression) ->
    structure_item -> Parsetree.structure_item
end
