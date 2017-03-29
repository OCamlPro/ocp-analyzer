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

open Hgraph_types
open Stack_types

module OneLevel (T:OrderedHashedType) : Stack with type elt = T.t

module TwoLevels (T:OrderedHashedType) : Stack with type elt = T.t

module Local (Top:Stack) : Stack with type elt = Top.elt

module Leveled (T:LeveledFunction) : Stack with type elt = T.t
