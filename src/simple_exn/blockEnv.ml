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
open Common_types
open Map_utils

module Make (ProgId: Set.OrderedType) =
struct
  module XIdm = Map.Make(ProgId)

  module Vals = AbsVals.Make(ProgId)

  module Offset = struct
    type t = int
    let compare = Pervasives.compare
    let print ppf i = Format.fprintf ppf "off_%d" i
  end

  module OM = Map.Make(Offset)

  type tag = Tag_int of int | Tag_fun of F.t | Tag_array

  (* The unique field denotes whether we know it refers to a single block,
     or assume it can represent different memory allocations *)
  type block =
    {
      tag: tag;
      args: Vals.t OM.t;
      unique: bool;
    }

  let block_is_bottom bl =
    OM.for_all (fun _ -> Vals.is_bottom) bl.args

  (* A variable not present in the block map has bottom semantics. *)
  type t = block XIdm.t

  let bottom = XIdm.empty

  let is_bottom e =
    XIdm.for_all
      (fun _ bl -> block_is_bottom bl)
      e

  let var_is_bottom xid e =
    try
      let bl = XIdm.find xid e in
      block_is_bottom bl
    with
    | Not_found -> true

  let rec set_top xid e =
    try
      let bl = XIdm.find xid e in
      OM.fold
        (fun _ vals acc ->
           try
             Vals.fold set_top vals acc
           with
           | Vals.Top_argument -> acc)
        bl.args
        e
    with
    | Not_found -> e

  (* Remove variable from the map *)
  (* TODO: examine the need the remove deep in the map *)
  let rec set_bottom xid e =
    XIdm.remove xid e

  let var_is_unique xid e =
    try
      let bl = XIdm.find xid e in
      Some bl.unique
    with
    | Not_found -> None

  let block_meet bx by =
    assert (bx.tag = by.tag);
    let args =
      OM.merge
        (map_merge_meet Vals.meet Vals.is_bottom)
        bx.args
        by.args
    in
    let unique = bx.unique || by.unique in
    {tag = bx.tag; args; unique}

  let meet =
    XIdm.merge (map_merge_meet block_meet block_is_bottom)

  let block_join join_fun bx by =
    assert (bx.tag = by.tag);
    let args =
      OM.merge
        (map_merge_join join_fun)
        bx.args
        by.args
    in
    let unique = bx.unique && by.unique in
    {tag = bx.tag; args; unique}

  let join =
    XIdm.merge (map_merge_join (block_join Vals.join))

  let widening =
    XIdm.merge (map_merge_join (block_join Vals.widening))

  let block_leq bx by =
    let module M =
      MapLeq (struct
        module M = OM type v = Vals.t let vleq = Vals.leq
      end)
    in
    M.comp bx.args by.args

  let blocks_leq xm ym =
    let module M =
      MapLeq (struct
        module M = XIdm type v = block let vleq = block_leq
      end)
    in
    M.comp xm ym

  let leq = blocks_leq

  let get_field i off e =
    try
      let bl = XIdm.find i e in
      OM.find off bl.args
    with
    | Not_found -> Vals.bottom

  let set_field_strong i off bl v e =
    XIdm.add i {bl with args = OM.add off v bl.args} e

  let set_field_weak i off bl v e =
    let nv =
      let prev = try OM.find off bl.args with Not_found -> Vals.bottom in
      Vals.join v prev
    in
    XIdm.add i {bl with args = OM.add off nv bl.args} e

  let set_field i off e v =
    try
      let bl = XIdm.find i e in
      if bl.unique
      then (* Strong update *)
        set_field_strong i off bl v e
      else
        set_field_weak i off bl v e
    with
    | Not_found -> e

  let may_set_field i off e v =
    try
      let bl = XIdm.find i e in
      set_field_weak i off bl v e
    with
    | Not_found -> e

  let duprecord i e j =
    try
      let bl = XIdm.find j e in
      XIdm.add i bl e
    with
    | Not_found -> e

  let may_duprecord i e j =
    try
      let bl = XIdm.find j e in
      try
        let bl_prev = XIdm.find i e in
        let bl_join = block_join Vals.join bl bl_prev in
        XIdm.add i bl_join e
      with
      | Not_found -> XIdm.add i bl e
    with
    | Not_found -> e

  let constr i e = function
    | Ccp _
    | Cbool _
    | Ctype _
    | Cstring _
    | Cnotstring _
      -> failwith "Unsupported constraint"
    | Ctag n ->
      try
        let bl = XIdm.find i e in
        if bl.tag = Tag_int n then e
        else bottom
      with
      | Not_found -> bottom

  let has_tag tag i e =
    try
      let bl = XIdm.find i e in
      bl.tag = tag
    with
    | Not_found -> false

  let get_fun i e =
    try
      let bl = XIdm.find i e in
      match bl.tag with
      | Tag_int _ | Tag_array -> None
      | Tag_fun f -> Some f
    with
    | Not_found -> None

  let allocate e xid alloc args =
    let rec make_args off acc = function
      | [] -> acc
      | arg :: tail ->
        make_args (off + 1) (OM.add off arg acc) tail
    in
    let add_block bl =
      try
        let prev_bl = XIdm.find xid e in
        let merge_args _ a b =
          match a,b with
          | None, x | x, None -> x
          | Some a, Some b -> Some (Vals.join a b)
        in
        assert (bl.tag = prev_bl.tag);
        let bl_join =
          { tag = bl.tag;
            args = OM.merge merge_args bl.args prev_bl.args;
            unique = false;
          }
        in
        XIdm.add xid bl_join e
      with
      | Not_found ->
        XIdm.add xid bl e
    in
    match alloc with
    | XPmakearray _ ->
      let bl =
        { tag = Tag_array;
          args = make_args 0 OM.empty args;
          unique = true;
        }
      in
      add_block bl
    | XPmakeblock (tag, _) ->
      let bl =
        { tag = Tag_int tag;
          args = make_args 0 OM.empty args;
          unique = true;
        }
      in
      add_block bl
    | XPfun f ->
      let bl =
        { tag = Tag_fun f;
          args = make_args 0 OM.empty args;
          unique = true;
        }
      in
      add_block bl

  let print_tag ppf = function
    | Tag_int i -> Format.fprintf ppf "%d" i
    | Tag_fun f -> Format.fprintf ppf "%a" F.print f
    | Tag_array -> Format.fprintf ppf "Array"

  let print_args ppf args =
    OM.print Vals.print ppf args

  let print_block ppf bl =
    Format.fprintf ppf "@[Tag:@ %a@.Args:@ %a@]"
      print_tag bl.tag
      print_args bl.args

  let print ppf e =
    XIdm.print print_block ppf e

  let print_var e ppf x =
    try
      let bl = XIdm.find x e in
      print_block ppf bl
    with
    | Not_found -> Format.fprintf ppf "<Bot>"
end
