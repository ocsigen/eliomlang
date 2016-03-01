open Typedtree

val get_section_side :
  structure_item_desc ->
  structure_item_desc * [> `Client | `Server | `Shared ] option

val unfold_expression :
  expression ->
  ([> `Expr of expression
   | `Fragment of attribute list * 'a
   | `Injection of attribute list * 'a ]
   as 'a)
