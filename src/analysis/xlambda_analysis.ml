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
open Xlambda_to_hgraph
module G = G

type v = Vertex.t
type h = Hedge.t
type ha = Xlambda_to_hgraph.hattr

let apply_counter = ref 0
let get_counter () = !apply_counter
      

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
  val g : hg
  val funs : ( F.t, Xlambda_to_hgraph.fun_desc ) Hashtbl.t
  val mk_vertex : unit -> v
  val mk_hedge : unit -> Hedge.t
end

module Stack = Abstract_stack.Leveled ( F )

module M :
  functor ( A : functor (Stack:Stack_types.Stack) ->
      OCamlEnv.S with type hedge = Hedge.t
                  and module Stack = Stack) ->
  functor ( E : Entry ) ->
sig
  include Fixpoint_types.Manager
    with module T := T
     and module H = G
     and type function_id = F.t
     and module Function_id = F
     and module Stack = Stack
     and type hedge_attribute = hattr
     and type vertex_attribute = vattr
     and type graph_attribute = gattr
     and type abstract = A(Stack).t
     and type global = A(Stack).global
end
  =
  functor ( A : functor (Stack:Stack_types.Stack) ->
      OCamlEnv.S with type hedge = Hedge.t
                  and module Stack = Stack) ->
  functor ( E : Entry ) ->
  struct

    module H = Xlambda_to_hgraph.G

    open H

    module A = A(Stack)

    type abstract = A.t
    type global = A.global

    let global_init () = A.init_global

    let bottom _ = A.bottom
    let is_bottom _ = A.is_bottom
    let is_leq _ = A.leq
    let join_list _ = List.fold_left A.join A.bottom
    let abstract_init v = if v = E.inv then A.empty else A.bottom
    let widening _ = A.widening
    let narrowing = None

    type hedge_attribute = hattr
    type vertex_attribute = vattr
    type graph_attribute = gattr

    type function_id = F.t
    module Function_id = F
    let find_function fid =
      let f = Hashtbl.find E.funs fid in
      Array.iter (fun v -> assert(not (H.VertexSet.mem v f.f_vertex))) f.f_in;
      f.f_graph, {
        sg_input = f.f_in;
        sg_output = f.f_out;
        sg_vertex = f.f_vertex;
        sg_hedge = f.f_hedge;
      }

    module Stack = Stack

    let apply
        (hedge_id : h )
        ( l : ha )
        ( s : Stack.t )
        ( envs : abstract array )
        global =
      incr apply_counter;
      let doubleout (e,e',global) = ( [| e; e' |], global, [] ) in
      let in_apply ( x, action) env global =
        match action with
        | App | App_prep _ | App_return | App_exn | Return _ | Retexn _
        | EndLet _ | Alloc _ | Lazyforce _ | Ccall _ | Send _ -> assert false
        | Var i -> A.var ~hedge:hedge_id ~s ~x ~env ~global i
        | Const c -> A.const ~hedge:hedge_id ~s ~x ~env ~global c
        | Prim ( p, l ) -> A.primitive ~hedge:hedge_id ~s ~x ~env ~global p l
        | Constraint c -> A.constr ~hedge:hedge_id ~s ~x ~env ~global c
      in
      let extract_allocs l =
        let rec aux acc = function
          | [] -> (List.rev acc, [])
          | ([x], Alloc (aid,a,l)) :: tail -> aux ((x,aid,a,l)::acc) tail
          | (_, Alloc (_,_,_)) :: _ -> assert false
            (* Allocations should go in only one value *)
          | l -> (List.rev acc, l)
        in
        aux [] l
      in
      assert ( Array.length envs = 1 );
      let env = envs.(0) in
      (* let rec aux e global l = *)
      (*   match l with *)
      (*   | [] -> e, global *)
      (*   | h :: t -> *)
      (*     let e, global = in_apply h e global in *)
      (*     aux e global t *)
      (* in  *)
      let rec do_apply (env, global) l =
        if A.is_bottom env then
          [|A.bottom|],global,[]
        else
          match l with
          | [] ->
            [|env|],global,[]
          | ( [x], App_prep ( f, arg ) ) :: tail ->
            do_apply ( A.app_prep ~hedge:hedge_id ~s ~x ~env ~global ~f ~arg ) tail
          | ( [x], App_return ) :: tail ->
            do_apply (A.app_return ~hedge:hedge_id ~s ~x ~env ~global ) tail
          | ( [x], App_exn ) :: tail ->
            do_apply (A.app_exn ~hedge:hedge_id ~s ~x ~env ~global ) tail
          | ( _, Alloc _) :: _ ->
            let allocs, tail = extract_allocs l in
            do_apply (A.allocations ~hedge:hedge_id ~s ~env ~global allocs ) tail
          | (_, (App | Ccall (_,_) | Lazyforce _ | Send (_,_))) :: _ ->
            assert false (* All primitives with double output should appear
                            alone in the list. *)
          | ( [], EndLet l) :: tail ->
            do_apply (A.endlet ~hedge:hedge_id ~s ~env ~global l) tail
          | ( [], Return id) :: tail ->
            do_apply (A.return ~hedge:hedge_id ~s ~env ~global id) tail
          | ( [], Retexn id) :: tail ->
            do_apply (A.retexn ~hedge:hedge_id ~s ~env ~global id) tail
          | ( [x], action) :: tail ->
            do_apply (in_apply (x,action) env global) tail
          | _ -> assert false  (* Wrong number of outputs to an hedge *)
      in
      match l with
      | [ [x;e], App ] ->
        let fl, global = A.app ~hedge:hedge_id ~s ~x ~e ~env ~global in
        [| A.bottom; A.bottom |], global, fl
      | [ [x;e], Ccall (pd, l) ] ->
        doubleout (A.ccall ~hedge:hedge_id ~s ~x ~e ~env ~global pd l)
      | [ [x;e], Lazyforce l] ->
        doubleout (A.lazy_force ~hedge:hedge_id ~s ~x ~e ~env ~global l)
      | [ [x;e], Send (o, m) ] ->
        doubleout (A.send ~hedge:hedge_id ~s ~x ~e ~env ~global o m)
      | (_, (App | Ccall (_,_) | Lazyforce _ | Send (_,_))) :: _ ->
         assert false (* All primitives with double output should appear alone
                         in the list. *)
      | l -> do_apply (env, global) l

  end
