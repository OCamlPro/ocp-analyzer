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

let set_true d =
  { d with
    int = Int_interv.join
        ( Int_interv.meet (Int_interv.at_least 1) d.int)
        ( Int_interv.meet (Int_interv.at_most (-1)) d.int);
    cp = Ints.remove 0 d.cp;
  }

let can_be_true env d =
  not ( is_bottom (set_true d) )

let set_false d =
  let dfalse =
    { bottom with
      int = Int_interv.cst 0;
      cp = Ints.singleton 0;
      expr = d.expr;
    } in
  if d.top
  then dfalse
  else { bottom with
         int = Int_interv.meet dfalse.int d.int;
         cp = Ints.inter dfalse.cp d.cp; }

let can_be_false env { top; int; cp; _ } =
  top ||
  Int_interv.mem 0 int ||
  Ints.mem 0 cp

