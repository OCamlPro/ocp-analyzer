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

open Utils
open Locations

module Intm = Map.Make
    (struct
      type t = int
      let compare (x:t) y = Pervasives.compare x y
      let print = Format.pp_print_int
    end)

type tag = int

module Tagm =
  Map.Make (
  struct
    type t = tag
    let compare = compare
    let print = Format.pp_print_int
  end)


type t = Locs.t array Intm.t Tagm.t


let map_join_helper f _ a b =
  match a, b with
  | x, None | None, x -> x
  | Some x, Some y -> Some ( f x y )

let array_merge s1 s2 =
  Array.mapi (fun i i1 -> Locs.union i1 s2.(i)) s1

let join b b' =
  Tagm.merge
    (map_join_helper
       ( Intm.merge
           ( map_join_helper array_merge )
       )
    ) b b'

let map_meet_helper f g _ a b =
  match a,b with
  | _, None | None, _ -> None
  | Some a, Some b ->
    let res = f a b in
    if g res
    then None
    else Some res

let array_inter a b =
  Array.mapi (fun i i1 -> Locs.inter i1 b.(i)) a

let array_has_empty a =
  let l = Array.length a in
  let rec aux i =
    i < l && ( Locs.is_empty a.(i) || aux (succ i) )
  in aux 0

let meet b b' =
  Tagm.merge
    (map_meet_helper
       ( Intm.merge
           (map_meet_helper array_inter array_has_empty) )
       Intm.is_empty)
    b b'

let widening = join

let bottom = Tagm.empty
let is_bottom =
  Tagm.for_all
    (fun _ -> Intm.for_all
        (fun _ -> array_has_empty)
    )

let array2_leq a b =
  let l = Array.length a in
  let rec aux i =
    i < l && ( Locs.subset a.(i) b.(i) && aux (succ i))
  in aux 0

let leq b b' =
  try
    Tagm.for_all
      (fun tm b ->
         let b' = Tagm.find tm b' in
         Intm.for_all
           (fun s b ->
              let b' = Intm.find s b' in
              array2_leq b b') b
      ) b
  with Not_found -> false

let equal b b' = leq b b' && leq b' b (* TODO : can be enhanced *)
  
