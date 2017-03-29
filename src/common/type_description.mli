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

type modpath =
  | Base of string
  | Dot of modpath * string * int
  | Apply of modpath * modpath

type typ_decl =
  { typarams : string option list;
    tyarity : int;
    tykind : typ_kind;
    tymanifest : typ_desc option;
  }

and typ_kind =
    Tyabstract
  | Tyrecord of label list
  | Tyvariant of constr list
  | Tyopen

and label =
  { lid : string;
    lmut : bool;
    ltyp : typ_desc;
  }

and constr =
  { cid : string;
    cargs : typ_desc list;
    cres : typ_desc option;
  }

and typ_constr =
  { tyname : modpath;
    tydecl : typ_decl;
  }

and typ_desc =
  | Tyvar of string option
  | Tyarrow of typ_desc * typ_desc
  | Tytuple of typ_desc list
  | Tyconstr of typ_constr * typ_desc list
  | Tyother
