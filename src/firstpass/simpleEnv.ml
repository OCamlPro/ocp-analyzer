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
open BaseTypes
  
module Fs = CurrySet.Make (F)
module XidFs = CurrySet.Nest (Xid) (Fs)

type t =
  {
    funs: XidFs.t option;
    constraints : int Xidm.t;
  }
    

let bottom = { funs: None; constraints: Xidm.empty; }
let is_bottom = function { funs = None; _} -> true | _ -> false
let empty = { funs = Some XidFs.empty; constraints = Xidm.empty; }

let meet _ _ = failure "meet not implemented"

let join a b = match a,b with
  | { funs = None; _}, x | x, { funs = None; _} -> x
  | { funs = Some af; constraints = ac;}, { funs = Some bf; constraints = bc; } ->
    { funs = Some (XidFs.union af bf);
      constraints = Xidm.merge
          (fun a b ->
             match a,b with
             | Some a, Some b when a = b -> a
             | _,_ -> None) ac bc;
    }

let widening = join

let leq a b =
  match a.funs,b.funs with
  | None, _ -> true
  | _, None -> false
  | Some af, Some bf ->
    XidFs.subset af bf &&
    (try Xidm.for_all (fun xb i -> Xidm.find xb a.constraints = i) b.constraints
     with Not_found -> false)

type global = AllocAutomaton.t

let init_global : global = AllocAutomaton.empty

module AA = AllocAutomaton

let primitive ~hedge ~x ~env ~global p l =
  match p,l with
  | XPfield i, [b] ->
  | XPfloatfield i, [b] -> 
    begin
      try let t = Xidm.find x env.constraints in
        env, AA.link_tag_field b t i x global
      with Not_found -> env, AA.link_field b i x global
    end
  | XPsetfield (i,mut) [b;v] ->
  | XPsetfloatfield (i,mut), [b;v] ->
    begin
      try let t = Xidm.find b env.constraints in
        env, AA.link_tag_field ~mut b t i v global
      with Not_found -> env, AA.link_field ~mut b i v global
    end
  | XPduprecord (trr,i), [b] -> env,AA.duprecord x trr i b global
  | XParrayrefu k, [a] -> failwith "Todo: arrayrefu"
  | XParraysetu k, [a,v] -> failwith "Todo: arraysetu"
  | XPgetfun f, [v] -> failwith "Todo: getfun"
  | XPfunfield i, [f] -> failwith "Todo: funfield"
  | XPgetarg, [f] -> failwith "Todo: getarg"

let var ~hedge ~x ~env ~global y = env, AA.link_vars x y global
let const ~hedge ~x ~env ~global _ = env, global
let constr ~hedge ~x ~env ~global = function
  | Ccp _ | Cbool _ -> env, global
  | Ctag i -> { env with consraints = Xidm.add x i env.constraints; }

let allocations ~hedge ~env ~gobal l = failwith "Todo: allocations"

let ccall ~hedge ~x ~env ~global p l =  failwith "Todo: ccall"
let lazy_force ~hedge ~x ~env ~global y = failwith "Todo: lazyforce"
let send ~hedge ~x ~env ~global o m =  failwith "Todo: send"

let app_prep ~hedge ~x ~env ~global ~f ~arg = 
let app_return ~hedge ~x ~env ~global =  failwith "Todo: app_return"
let app_exn ~hedge ~x ~env ~global =  failwith "Todo: app_exn"
let app ~hedge ~x ~env ~global =  failwith "Todo: app"
let return ~hedge ~x ~env ~global y =  failwith "Todo: return"
let retexn ~hedge ~x ~env ~global y =  failwith "Todo: retexn"
