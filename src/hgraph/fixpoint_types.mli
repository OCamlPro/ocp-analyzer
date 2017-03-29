(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

open Hgraph_types

module type Manager = sig

  module T : T
  module H : Hgraph with module T := T
  open H

  type abstract
  type global

  val bottom : T.vertex -> abstract
  val is_bottom : T.vertex -> abstract -> bool
  val is_leq : T.vertex -> abstract -> abstract -> bool
  (* val join : vertex -> abstract -> abstract -> abstract *)
  val join_list : T.vertex -> abstract list -> abstract
  val widening : T.vertex -> abstract -> abstract -> abstract
  val narrowing : (T.vertex -> abstract -> abstract -> abstract) option

  val abstract_init : T.vertex -> abstract
  val global_init : unit -> global

  type vertex_attribute
  type hedge_attribute
  type graph_attribute
  type function_id

  module Function_id : OrderedHashedType with type t = function_id
  module Stack : Stack_types.Stack with type elt = function_id

  val find_function : function_id ->
    (vertex_attribute, hedge_attribute, graph_attribute) graph * subgraph

  val apply : T.hedge -> hedge_attribute -> Stack.t -> abstract array -> global ->
    abstract array * global * function_id list

end
