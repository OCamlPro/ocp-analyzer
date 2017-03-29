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
open Fixpoint_types

type ('a,'b,'c) vertex_result_attribute =
  { v_orig : 'a;
    v_abstract : 'b;
    v_stack : 'c }

type ('a,'b,'c) hedge_result_attribute =
  { h_orig : 'a;
    h_abstract : 'b;
    h_stack : 'c }

module Fixpoint (T:T) (Manager:Manager with module T := T) : sig

  type input_graph = (Manager.vertex_attribute,
                      Manager.hedge_attribute,
                      Manager.graph_attribute)
      Manager.H.graph

  type output_vattr = (Manager.vertex_attribute,
                       Manager.abstract,
                       Manager.Stack.t)
    vertex_result_attribute

  type output_hattr = (Manager.hedge_attribute,
                       Manager.abstract array option,
                       Manager.Stack.t)
    hedge_result_attribute

  type output_graph = (output_vattr, output_hattr, Manager.global) Manager.H.graph

  val kleene_fixpoint :
    (* ?err_graph:(unit, Manager.hedge_attribute, unit) Manager.H.graph option ref -> *)
    input_graph -> Manager.H.VertexSet.t ->
    output_graph * Manager.H.VertexSet.t Manager.H.VertexMap.t

  (* returns the unfolded graph and a map associating original vertex
     with new ones *)

end
