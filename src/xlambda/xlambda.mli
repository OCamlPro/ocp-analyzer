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

open Common_types

type xlambda =
| Xlet of xlet
| Xrec of xrec
| Xend of xid

and xlet =
{
  xe_id : xid;
  xe_lam : xcontrol;
  xe_kind : Lambda.let_kind;
  xe_in : xlambda;
  xe_floc : Location.t option;
  xe_loc : Location.t option;
}

and xrec =
{
  xr_decls : ( xid * allocator * xid list ) list;
  xr_in : xlambda;
}

and xcontrol =
| Xvar of xid
| Xconst of Lambda.structured_constant
| Xapply of xid * xid
| Xprim of primitive * xid list
| Xalloc of allocator * xid list
| Xswitch of xid * xswitch
| Xstringswitch of xid * ( string * xlambda ) list * xlambda option
| Xstaticraise of int * xid list
| Xstaticcatch of xlambda * (int * xid list) * xlambda
| Xraise of Lambda.raise_kind * xid
| Xtrywith of xlambda * xid * xlambda
| Xifthenelse of xid * xlambda * xlambda
| Xwhile of xlambda * xlambda
| Xfor of xid * xid * xid * direction_flag * xlambda
(* | Xassign of xid * xid *)
| Xlazyforce of xid
| Xccall of Primitive.description * xid list
| Xsend of Lambda.meth_kind * xid * xid

and xswitch =
{
  x_numconsts: int;
  x_consts: (int * xlambda) list;
  x_numblocks: int;
  x_blocks: (int * xlambda) list;
  x_failaction: xlambda option;
}
