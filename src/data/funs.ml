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
open Data

let field i f =
  Fm.fold (fun _ a locs -> Locs.union a.(i) locs) f.f Locs.empty

let has_fid i fu = Fm.mem i fu.f

let fid i fu =
  assert(Fm.mem i fu.f);
  { bottom with f = Fm.singleton i ( Fm.find i fu.f ); }

let mk i l =
  { bottom with
    f = Fm.singleton i ( Array.of_list l );
  }

let extract_ids { f; _ } acc =
  Fm.fold (fun k _ l -> k::l) f acc
