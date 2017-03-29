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

module C = MakeId(struct end) 

let tbl : ( C.t, string ) Hashtbl.t = Hashtbl.create 1024

let to_string = Hashtbl.find tbl

module Constant =
struct
  include C
  let print pp c = Format.pp_print_string pp (to_string c)
end

module Constants = Set.Make (struct include Constant end)

type simple = Top | Constants of Constants.t
type t = simple


let mk s =
  let c = Constant.create () in
  Hashtbl.add tbl c s;
  c

let singleton s =
  Constants ( Constants.singleton ( mk s ) )

let join a b =
  match a,b with
  | Top, _ | _, Top -> Top
  | Constants a, Constants b -> Constants (Constants.union a b)
let meet a b =
  match a,b with
  | Top, x | x, Top -> x
  | Constants a, Constants b -> Constants (Constants.inter a b)
let widening = join

let is_bottom = function
  | Constants a -> Constants.is_empty a
  | Top -> false

let bottom = Constants Constants.empty

let leq a b =
  match a,b with
  | _, Top -> true
  | Top, _ -> false
  | Constants a, Constants b -> Constants.subset a b
