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

open Locations
open Data
open ValueMap

(* The environment *)
type t =
  | Bottom
  | Env of t'
and t' = 
  {
    entries: locs XIdm.t;
    values : ValueMap.t;
  }


(* Environment management *)

let is_bottom = function
  | Bottom -> true
  | _ -> false

let bottom = Bottom
let empty =
  Env {
    entries = XIdm.empty;
    values = ValueMap.empty;
  }

let no_bottom str f = function
  | Bottom -> Format.eprintf "Bottom error on %s@." str; assert false
  | Env e -> f e

let (!!) = function
  | Bottom -> assert false
  | Env e -> e

let (>!) e f = f (!!e)
let (>?) e f = match e with
  | Bottom -> Bottom
  | Env e -> f e

let (>>?) e def f = match e with
  | Bottom -> def
  | Env e -> f e

let (!>) e = Env e

(* Joining and widening helper *)

let merger_locs = merger_rec Locs.union

let merger_simple x y = merger_rec (fun a b -> assert (a=b); a) x y

let join_or_widen (union: Data.t -> Data.t -> Data.t) e1 e2 =
  match e1, e2 with
  | Bottom, e | e, Bottom -> e
  | Env d1, Env d2 ->
    let entries = XIdm.merge merger_locs d1.entries d2.entries in
    let values = join_or_widen union d1.values d2.values in
    Env { values; entries }


(* Environment joining *)

let join e1 e2 = join_or_widen union e1 e2
let (>+) = join

(* Environment joining with widening *)

(* total location set *)

module Atpls = Utils.Set.Make (
  struct
    type t = atpl
    let compare = Pervasives.compare
    let print = print_atpl
  end)


let widening e1 e2 =
  let fold_helper loc e loct =
    Locm.fold_key
      (fun loc d (dt,loct) -> Data.union d dt, Atpls.add loc loct)
      loc e.values (Data.bottom, loct)
  in
  match e1, e2 with
  | Bottom, a | a, Bottom -> a
  | Env e1, Env e2 ->
    let entries = XIdm.merge merger_locs e1.entries e2.entries in
    let values = Locm.fold_by_loc
        (fun loc values ->
           let d1, loct = fold_helper loc e1 Atpls.empty in
           let d2, loct = fold_helper loc e2 loct in
           let d3 = Data.widening d1 d2 in
           Atpls.fold (fun loc values -> Locm.add loc d3 values) loct values
        ) e1.values e2.values in
    !> { entries; values; }

