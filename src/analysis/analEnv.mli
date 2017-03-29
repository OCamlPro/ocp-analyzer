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

open Common_types

module type EnvCommon =
sig

  type t
  type hedge

  (* empty environments *)
  val bottom : t
  val is_bottom : t -> bool
  val empty : t

  (* lattice handlers *)
  val meet : t -> t -> t
  val join : t -> t -> t
  val widening : t -> t -> t
  val leq : t -> t -> bool
  (* val narrowing : t -> t -> t *)
  
end
  
module type S =
sig

  type t

  include EnvCommon with type t := t

  module Stack : Stack_types.Stack

  (* simple value manipulations *)
  val primitive : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> primitive -> xid list -> t
  val var : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> xid -> t
  val const : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> Lambda.structured_constant -> t
  val constr : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> constr -> t

  (* memory allocations *)
  val allocations : hedge:hedge -> s:Stack.t -> env:t -> ( xid * AllocationId.t * allocator * xid list ) list -> t

  (* potentially exn-raising manipulations *)
  val ccall : hedge:hedge -> s:Stack.t -> x:xid -> e:xid -> env:t -> Primitive.description -> xid list -> t * t
  val lazy_force : hedge:hedge -> s:Stack.t -> x:xid -> e:xid -> env:t -> xid -> t * t
  val send : hedge:hedge -> s:Stack.t -> x:xid -> e:xid -> env:t -> xid -> xid -> t * t

  (* function call convention *)
  val app_prep : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> f:xid -> arg:xid -> t
  val app_return : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> t
  val app_exn : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> t
  val app : hedge:hedge -> s:Stack.t -> x:xid -> e:xid -> env:t -> F.t list
  val return : hedge:hedge -> s:Stack.t -> env:t -> xid -> t
  val retexn : hedge:hedge -> s:Stack.t -> env:t -> xid -> t

  (* notification of variables going out of scope *)
  val endlet : hedge:hedge -> s:Stack.t -> env:t -> xid list -> t

end

module type Make =
  functor (H:Hgraph_types.CloneOrderedHashedType) ->
  functor (Stack: Stack_types.Stack) ->
    S with type hedge = H.t and module Stack = Stack
