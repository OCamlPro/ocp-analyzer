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

open Data

let any i =
  let rec aux res = function
    | 0 -> res
    | n      -> let n = pred n in aux (Ints.add n res) n
  in { bottom with cp = aux Ints.empty i }

let singleton i =
  { bottom with cp = Ints.singleton i }


let has v d = Ints.mem v d.cp || Int_interv.mem v d.int

let is_one d =
  let d = Int.import_cp d in
  (Int_interv.unique d.int) &&
  is_bottom { d with int = bottom.int }
  

let restrict ?v d =
  match v with
    Some v -> { (singleton v) with expr = d.expr }
  | None -> { bottom with cp = d.cp; expr = d.expr }

