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

(* Regular language for describing stacks *)
type 'a stack_expr =
  | Start (* The initial, empty stack *)
  | Any (* A single stack frame, containing any element *)
  | Elt of 'a (* A single stack frame, with a specific element *)
  | Concat of 'a stack_expr list (* Concatenation *)
               (* The head of the list is the top of the stack *)
  | Disjunct of 'a stack_expr list (* Disjunction *)
  | Star of 'a stack_expr (* Repetition *)

module type Stack = sig

  include Hgraph_types.OrderedHashedType

  type elt

  val empty : t
  val push : t -> elt -> t

  val to_stack_expr : t -> elt stack_expr

end

module type LeveledFunction = sig
  include Hgraph_types.OrderedHashedType
  val is_important : t -> bool
  val n : int
end
