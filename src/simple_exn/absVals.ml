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

module Make (ProgId: Set.OrderedType) =
struct
  module XIds = Set.Make(ProgId)

  type t = S of XIds.t | Top

  exception Top_argument

  let bottom = S XIds.empty

  let is_bottom = function
    | Top -> false
    | S s -> XIds.is_empty s

  let join x y =
    match x,y with
    | Top, _ | _, Top -> Top
    | S sx, S sy -> S (XIds.union sx sy)

  let meet x y =
    match x,y with
    | Top, z | z, Top -> z
    | S sx, S sy -> S (XIds.inter sx sy)

  let equal x y =
    match x,y with
    | Top, Top -> true
    | S sx, S sy -> XIds.equal sx sy
    | _, _ -> false

  let leq x y =
    match x,y with
    | _, Top -> true
    | Top, S _ -> false
    | S sx, S sy -> XIds.subset sx sy

  let widening_t threshold x y =
    match join x y with
    | Top -> Top
    | S s -> if XIds.cardinal s > threshold then Top else S s

  let widening = (* widening_t 4 *)
    join (* Works since we do not create xids *)

  let mem xid = function
    | Top -> true
    | S s -> XIds.mem xid s

  let fold f x init =
    match x with
    | Top -> raise Top_argument
    | S s -> XIds.fold f s init

  let singleton xid = S (XIds.singleton xid)

  let as_singleton = function
    | Top -> None
    | S s -> if XIds.cardinal s = 1 then Some (XIds.choose s) else None

  let filter f = function
    | Top -> Top
    | S s -> S (XIds.filter f s)

  let exists f = function
    | Top -> true
    | S s -> XIds.exists f s

  let for_all f = function
    | Top -> false
    | S s -> XIds.for_all f s

  let print ppf = function
    | Top -> Format.fprintf ppf "<Top>"
    | S s -> Format.fprintf ppf "@[{%a}@]"
               (XIds.print_sep (fun ppf -> Format.fprintf ppf ";@ ")) s

  let print_abs printer ppf = function
    | Top -> Format.fprintf ppf "<Top>"
    | S s -> XIds.iter (fun x -> Format.fprintf ppf "%a;@." printer x) s
end
