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

module type S =
sig
  type t
  type global
      
  include AnalEnv.EnvCommon with type t := t

  module Stack : Stack_types.Stack

  val init_global : global

  (* simple value manipulations *)
  val primitive : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> global:global -> primitive -> xid list -> t * global
  val var : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> global:global -> xid -> t * global
  val const : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> global:global -> Lambda.structured_constant -> t * global
  val constr : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> global:global -> constr -> t * global

  (* memory allocations *)
  val allocations : hedge:hedge -> s:Stack.t -> env:t -> global:global -> ( xid * AllocationId.t * allocator * xid list ) list -> t * global

  (* potentially exn-raising manipulations *)
  val ccall : hedge:hedge -> s:Stack.t -> x:xid -> e:xid -> env:t -> global:global -> Primitive.description -> xid list -> t * t * global
  val lazy_force : hedge:hedge -> s:Stack.t -> x:xid -> e:xid -> env:t -> global:global -> xid -> t * t * global
  val send : hedge:hedge -> s:Stack.t -> x:xid -> e:xid -> env:t -> global:global -> xid -> xid -> t * t * global

  (* function call convention *)
  val app_prep : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> global:global -> f:xid -> arg:xid -> t * global
  val app_return : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> global:global -> t * global
  val app_exn : hedge:hedge -> s:Stack.t -> x:xid -> env:t -> global:global -> t * global
  val app : hedge:hedge -> s:Stack.t -> x:xid -> e:xid -> env:t -> global:global -> F.t list * global
  val return : hedge:hedge -> s:Stack.t -> env:t -> global:global -> xid -> t * global
  val retexn : hedge:hedge -> s:Stack.t -> env:t -> global:global -> xid -> t * global

  (* notification of variables going out of scope *)
  val endlet : hedge:hedge -> s:Stack.t -> env:t -> global:global -> xid list -> t * global

end

module Make
    ( M : AnalEnv.Make )
    ( H : Hgraph_types.CloneOrderedHashedType )
    ( Stack : Stack_types.Stack )
  : S with type t = M(H)(Stack).t
       and type global = unit
       and type hedge = H.t
       and module Stack = Stack
  =
  struct

    type global = unit
    let init_global = ()

    module E = M(H)(Stack)

    module Stack = E.Stack

    include (E : AnalEnv.EnvCommon with type t = E.t and type hedge = H.t)

    let primitive ~hedge ~s ~x ~env ~global p l = E.primitive ~hedge ~s ~x ~env p l, ()
    let var  ~hedge ~s ~x ~env ~global y = E.var ~hedge ~s ~x ~env y, ()
    let const ~hedge ~s ~x ~env ~global c = E.const ~hedge ~s ~x ~env c, ()
    let constr ~hedge ~s ~x ~env ~global c = E.constr ~hedge ~s ~x ~env c, ()

    let allocations ~hedge ~s ~env ~global l = E.allocations ~hedge ~s ~env l, ()

    let tuple_add_unit (a,b) = (a,b,())
    
    let ccall ~hedge ~s ~x ~e ~env ~global p l = tuple_add_unit @@ E.ccall ~hedge ~s ~x ~e ~env p l
    let lazy_force ~hedge ~s ~x ~e ~env ~global y = tuple_add_unit @@ E.lazy_force ~hedge ~s ~x ~e ~env y
    let send ~hedge ~s ~x ~e ~env ~global o m = tuple_add_unit @@ E.send ~hedge ~s ~x ~e ~env o m

    let app_prep ~hedge ~s ~x ~env ~global ~f ~arg = E.app_prep ~hedge ~s ~x ~env ~f ~arg, ()
    let app_return ~hedge ~s ~x ~env ~global = E.app_return ~hedge ~s ~x ~env, ()
    let app_exn ~hedge ~s ~x ~env ~global = E.app_exn ~hedge ~s ~x ~env, ()
    let app ~hedge ~s ~x ~e ~env ~global = E.app ~hedge ~s ~x ~e ~env, ()
    let return ~hedge ~s ~env ~global y = E.return ~hedge ~s ~env y, ()
    let retexn ~hedge ~s ~env ~global y = E.retexn ~hedge ~s ~env y, ()

    let endlet ~hedge ~s ~env ~global l = E.endlet ~hedge ~s ~env l, ()
  end
