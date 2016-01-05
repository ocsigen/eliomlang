open El_utils

module Shared : sig

  type 'a t = { client : 'a; server : 'a; }

  val expression :
    loc:Location.t -> Parsetree.expression -> Parsetree.expression

  val structure_item : Parsetree.structure_item -> Parsetree.structure_item t
  val signature_item : Parsetree.signature_item -> Parsetree.signature_item t

end

val mapper : Context.shared Ppx_core.Ast_traverse.map_with_context

val mapper' : string list -> Ast_mapper.mapper
