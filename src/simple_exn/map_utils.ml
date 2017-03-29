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

let map_merge_join mfun _ x y =
  match x,y with
  | None, x | x, None -> x
  | Some a, Some b -> Some (mfun a b)

let map_merge_meet mfun is_bot _ x y =
  match x,y with
  | None, x | x, None -> x
  | Some a, Some b ->
    let r = mfun a b in
    if is_bot r then None else Some r

module type MapLeqArg = sig
  module M : Map.S
  type v
  val vleq : v -> v -> bool
end

module MapLeq (M : MapLeqArg) = struct
  type t = M.v M.M.t
  let comp (x:t) (y:t) =
    M.M.for_all
      (fun k vx ->
         try
           let vy = M.M.find k y in
           M.vleq vx vy
         with
         | Not_found -> false)
      x
end
