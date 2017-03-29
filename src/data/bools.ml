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

let booleans = (Cps.any 2)

let restrict x =
  { bottom with cp = Ints.inter x.cp booleans.cp }

let notb x =
  { bottom with cp =
                  Ints.fold
                    (fun i res ->
                       match i with
                       | 0 -> Ints.add 1 res
                       | 1 -> Ints.add 0 res
                       | _ -> res ) x.cp Ints.empty;
  }

let of_bool b =
  Cps.singleton
    (
      if b
      then 1
      else 0
    )
