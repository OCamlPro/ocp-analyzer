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

open Locations


type array_descr =
  {
    elems: Locs.t;
    size: Int_interv.t;
    float: bool;
    gen: bool;
    addr: bool;
    int: bool;
  }

type t = array_descr

let join a1 a2 =
  {
    elems = Locs.union a1.elems a2.elems;
    size = Int_interv.join a1.size a2.size;
    float = a1.float || a2.float;
    gen = a1.gen || a2.gen;
    addr = a1.addr || a2.addr;
    int = a1.int || a2.int;
  }

let meet a1 a2 =
  {
    elems = Locs.inter a1.elems a2.elems;
    size = Int_interv.meet a1.size a2.size;
    float = a1.float && a2.float;
    gen = a1.gen && a2.gen;
    addr = a1.addr && a2.addr;
    int = a1.int && a2.int;
  }

let widening a1 a2 =
  { (join a1 a2) with
    size = Int_interv.widening a1.size a2.size;
  }

let bottom =
  {
    elems = Locs.empty;
    size = Int_interv.bottom;
    float = false;
    gen = false;
    addr = false;
    int = false;
  }

let is_bottom { elems; size; float; gen; addr; int; } =
  Locs.is_empty elems ||
  Int_interv.is_bottom size

let leq a b =
  Locs.subset a.elems b.elems &&
  Int_interv.leq a.size b.size &&
  (b.float || not a.float) &&
  (b.gen || not a.gen) &&
  (b.addr || not a.addr) &&
  (b.int || not a.int)

let equal a b =
  Locs.equal a.elems b.elems &&
  Int_interv.equal a.size b.size &&
  (b.float = a.float) &&
  (b.gen = a.gen) &&
  (b.addr = a.addr) &&
  (b.int = a.int)
