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

open Constants
open Locations
open Data
open Envs



(* Inclusion test *)

let included_simple a b = match a, b with
  | Top, _ | _, Top -> true
  | Constants s, Constants s' -> Constants.exists ( fun a -> Constants.mem a s') s

let array2_forall f a b =
  let l = Array.length a in
  let rec aux i = i = l || f a.(i) b.(i) || aux (succ i) in
  aux 0

(* Leq test *)

let leq_simple a b =
  match a, b with
  | _, Top -> true
  | Top, _ -> false
  | Constants a, Constants b -> Constants.subset a b

(* Intersection *)

let intersection_simple a b = match a, b with
  | Top, a | a, Top -> a
  | Constants s, Constants s' ->
    Constants ( Constants.inter s s')

let mi f _ a b = match a,b with
  | _, None | None, _ -> None
  | Some a, Some b -> Some (f a b)

let intersect_noncommut a b =
  (* keeps the ids in a that are possibly compatible with b *)
  if a.top then b
  else if b.top then a
  else
    let blocks = 
      Tagm.merge
        (mi
           (fun is1 is2 ->
              Intm.merge
                (mi (fun s1 _ -> s1 ) ) is1 is2
           ) ) a.blocks b.blocks
    in
    let f = Fm.merge (mi (fun a _ -> a ) ) a.f b.f in
    { top = false;
      int = Int_interv.meet a.int b.int;
      float = intersection_simple a.float b.float;
      string = intersection_simple a.string b.string;
      i32 = intersection_simple a.i32 b.i32;
      i64 = intersection_simple a.i64 b.i64;
      inat = intersection_simple a.inat b.inat;
      cp = Ints.inter a.cp b.cp;
      blocks;
      arrays =
        {
          a_elems = a.arrays.a_elems (* TODO: see that again *);
          a_size = Int_interv.meet a.arrays.a_size b.arrays.a_size;
          a_gen = a.arrays.a_gen && b.arrays.a_gen;
          a_float = a.arrays.a_float && b.arrays.a_float;
          a_addr = a.arrays.a_addr && b.arrays.a_addr;
          a_int = a.arrays.a_int && b.arrays.a_int;
        };
      f;
      expr = Hinfos.empty;
    }

(* Environment comparison *)

let merge_loc loc e = Locm.fold_key (fun _ -> Data.union) loc e.values Data.bottom

(* boolean leq *)
let lb a b = b || not a

let array2_fold f acc a b =
  assert (Array.length a = Array.length b);
  let rec foldi acc n =
    if n = Array.length a
    then acc
    else foldi (f acc a.(n) b.(n)) (succ n)
  in
  foldi acc 0

let rec is_leq e1 e2 =
  match e1, e2 with
  | Bottom, _ -> true
  | _, Bottom -> false
  | Env e1, Env e2 -> is_leq' e1 e2

and is_leq' e1 e2 =
    try let _ = is_leq_env e1 e2 in true
    with Not_found -> false

and is_leq_env e1 e2 =
  XIdm.fold
    (fun xid locs acc ->
       let locs2 = XIdm.find xid e2.entries in
       is_leq_locs e1 e2 acc locs locs2
    ) e1.entries Atpls.empty

and is_leq_locs e1 e2 visited l1 l2 =
  if Locs.subset l1 l2
  then Locs.fold
      (fun loc visited ->
         if Atpls.mem loc visited
         then visited
         else
           let d1 = merge_loc loc e1 in
           let d2 = merge_loc (Locs.find loc l2) e2 in
           is_leq_data e1 e2 (Atpls.add loc visited) d1 d2
      ) l1 visited
  else raise Not_found

and is_leq_data e1 e2 visited a b =
  let ill = is_leq_locs e1 e2 in
  if
    b.top
    || not a.top
       && Int_interv.leq a.int b.int
       && leq_simple a.float b.float
       && leq_simple a.string b.string
       && leq_simple a.i32 b.i32
       && leq_simple a.i64 b.i64
       && leq_simple a.inat b.inat
       && Ints.subset a.cp b.cp
       && Int_interv.leq a.arrays.a_size b.arrays.a_size
       && ( b.arrays.a_gen
            || not a.arrays.a_gen
               && lb a.arrays.a_float b.arrays.a_float
               && lb a.arrays.a_addr b.arrays.a_addr
               && lb a.arrays.a_int b.arrays.a_int
          )
  then
    let visited = 
      Tagm.fold
        (fun k a visited ->
           let b = Tagm.find k b.blocks in
           Intm.fold
             (fun k a visited ->
                let b = Intm.find k b in
                array2_fold ill visited a b
             ) a visited
        ) a.blocks visited in
    let visited = ill visited a.arrays.a_elems b.arrays.a_elems in
    Fm.fold
      (fun k a visited ->
         let b = Fm.find k b.f in
         array2_fold ill visited a b
      ) a.f visited
  else raise Not_found
