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
module Fm = Map.Make (Common_types.F)

type t = Locs.t array Fm.t

let join f f' =
  Fm.merge (fun _ a b ->
      match a,b with
      | None, x | x, None -> x
      | Some a, Some b ->
        Some (Array.init (Array.length a) @@
              fun i -> Locs.union a.(i) b.(i))
    ) f f'

let meet f f' =
  Fm.merge (fun _ a b ->
      match a,b with
      | None, _ | _, None -> None
      | Some _, Some _ -> a) f f' (* TODO: not perfect *)

let widening = join

let leq f f' =
  Fm.for_all (fun fid a ->
      try let b = Fm.find fid f' in
        Array.iteri
          (fun i s ->
             if Locs.subset s b.(i)
             then raise Not_found
             else ()) a;
        true
      with Not_found -> false) f

let equal f f' = leq f f' && leq f' f (* TODO: optimize *)

let is_bottom = Fm.is_empty
let bottom = Fm.empty
        
