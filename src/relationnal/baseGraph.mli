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

open BaseTypes

type graph
module Node : Ident

val bottom : graph

val add_tree : path -> tree -> graph -> graph
val copy : path -> edge -> path -> graph -> graph
val cut : path -> graph -> graph

val get_terminal : path -> graph -> Terminal.t

val get_root : graph -> Node.t
val fold_on_node :
  ( edge -> Node.t -> 'acc -> 'acc ) ->
  Node.t -> graph ->
  'acc -> 'acc

val get_child : Node.t -> edge -> graph -> Node.t

module Join : DiffFolder
  with type t = graph
