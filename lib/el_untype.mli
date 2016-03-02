open Typedtree

val get_section_side :
  structure_item_desc ->
  structure_item_desc * [> `Client | `Server | `Shared ] option

type eliom_expr =
  | Expr of expression
  | Fragment of
      { attrs : attributes ; id : int ; expr : eliom_expr }
  | Injection of
      { attrs : attributes ; id : int ; expr : eliom_expr }

val unfold_expression : expression -> eliom_expr
