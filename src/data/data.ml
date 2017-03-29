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

(* The atomic types *)
open Utils
open Common_types
open Locations

type locs = Locs.t

type constant = Constants.t

open Constants

type tag = int

(* type f = F.t *)

module Ints =
  Set.Make (
  struct
    type t = int
    let compare = compare
    let print = Format.pp_print_int
  end)
module Intm =
  Map.Make (
  struct
    type t = int
    let compare = compare
    let print = Format.pp_print_int
  end)

module Tagm =
  Map.Make (
  struct
    type t = tag
    let compare = compare
    let print = Format.pp_print_int
  end)

module Fm = Map.Make (F)

module Hinfos =
  Set.Make (
  struct
    type t = hinfo
    let compare = compare
    let print _ _ = ()
  end)


type array_descr =
  {
    a_elems: locs;
    a_size: Int_interv.t;
    a_float: bool;
    a_gen: bool;
    a_addr: bool;
    a_int: bool;
  }

(* The data *)

type t =
  {
    top : bool;
    int : Int_interv.t;
    float : simple;
    string : simple;
    i32 : simple;
    i64 : simple;
    inat : simple;
    cp : Ints.t;
    blocks : locs array Intm.t Tagm.t; (* referenced by tag, then by size *)
    arrays : array_descr;
    f : locs array Fm.t;
    expr : Hinfos.t;
  }

let simple_bottom = Constants Constants.empty

let bottom =
  {
    top = false;
    int = Int_interv.bottom;
    float = simple_bottom;
    string = simple_bottom;
    i32 = simple_bottom;
    i64 = simple_bottom;
    inat = simple_bottom;
    cp = Ints.empty;
    blocks = Tagm.empty;
    arrays = {
      a_elems = Locs.empty; a_size = Int_interv.bottom;
      a_float = false; a_gen = false; a_addr = false; a_int = false;
    };
    f = Fm.empty;
    expr = Hinfos.empty;
  }

let top = { bottom with top = true; }

let is_top { top; _ } = top

(* Bottom test *)

let ignore_first f _ = f
let array_for_all f a =
  let rec aux i =
    if i = Array.length a
    then true
    else f i a.(i) && aux (succ i)
  in aux 0

let is_bottom_simple = function
  | Top -> false
  | Constants c -> Constants.is_empty c

let is_bottom
    { top; int; float; string; i32;
      i64; inat; cp; blocks; arrays = { a_elems; a_size; _ }; f; expr; } =
  top = false &&
  Int_interv.is_bottom int && is_bottom_simple float &&
  is_bottom_simple string &&
  is_bottom_simple i32 && is_bottom_simple i64 &&
  is_bottom_simple inat &&
  Ints.is_empty cp &&
  Tagm.for_all
    (ignore_first
       (Intm.for_all
          (ignore_first
             (array_for_all
                (ignore_first Locs.is_empty)
             ))
       )) blocks &&
  ( Locs.is_empty a_elems || Int_interv.is_bottom a_size ) &&
  Fm.is_empty f



(* Union *)

let map_merge_helper f _ a b =
  match a, b with
  | x, None | None, x -> x
  | Some x, Some y -> Some ( f x y )

let array_merge s1 s2 =
  Array.mapi (fun i i1 -> Locs.union i1 s2.(i)) s1

let block_merge b1 b2 =
  Tagm.merge
    (map_merge_helper
       ( Intm.merge
           ( map_merge_helper array_merge )
       )
    ) b1 b2

let array_descr_merge ~integers a1 a2 =
  {
    a_elems = Locs.union a1.a_elems a2.a_elems;
    a_size = integers a1.a_size a2.a_size;
    a_float = a1.a_float || a2.a_float;
    a_gen = a1.a_gen || a2.a_gen;
    a_addr = a1.a_addr || a2.a_addr;
    a_int = a1.a_int || a2.a_int;
  }

let union_simple a b = match a, b with
  | Top, _ | _, Top -> Top
  | Constants s, Constants s' -> Constants ( Constants.union s s')

let merge_template ~integers a b =
  if a.top || b.top
  then top
  else
    let blocks = block_merge a.blocks b.blocks in
    let arrays = array_descr_merge ~integers a.arrays b.arrays in
    let f = Fm.merge (map_merge_helper array_merge) a.f b.f in
    {
      top = false;
      int = integers a.int b.int;
      float = union_simple a.float b.float;
      string = union_simple a.string b.string;
      i32 = union_simple a.i32 b.i32;
      i64 = union_simple a.i64 b.i64;
      inat = union_simple a.inat b.inat;
      cp = Ints.union a.cp b.cp;
      blocks; arrays; f;
      expr = Hinfos.union a.expr b.expr;
    }


let union a b =
  merge_template ~integers:Int_interv.join a b

let unions l = List.fold_left union bottom l

let widening a b =
  merge_template ~integers:Int_interv.widening a b
