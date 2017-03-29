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

let set_a i v a =
  let a = Array.copy a in
  a.(i) <- v;
  a

let singleton tag content =
  { bottom with
    blocks =
      Tagm.singleton tag
        ( Intm.singleton
            ( Array.length content)
            content
        )
  }

let restrict ?tag ?has_field ?size d =
  let restrict_tag_size im =
    match has_field with
    | None -> im
    | Some f -> Intm.filter (fun k _ -> k > f) im
  in
  let restrict_tag im =
    match size with
    | None -> restrict_tag_size im
    | Some s -> Intm.singleton s ( Intm.find s im)
  in
  { bottom with
    blocks =
      (
        match tag with
        | None ->
          Tagm.map restrict_tag d.blocks
        | Some t ->
          Tagm.singleton t
            ( restrict_tag ( Tagm.find t d.blocks))
      );
    expr = d.expr;
  }

let fieldn_map f n b =
  { b with
    blocks =
      Tagm.mapi
        (fun t sizes ->
           Intm.mapi
             (fun s a ->
                let a' = Array.copy a in
                a'.(n) <-
                  Locs.fold
                    (fun e -> Locs.add (f t s e))
                    a.(n) Locs.empty;
                a'
             ) sizes
        ) b.blocks;
  }

let has_tag t d = Tagm.mem t d.blocks
let is_one_tag d =
  Tagm.cardinal d.blocks = 1 &&
  is_bottom { d with blocks = bottom.blocks }

let sets_field i locs b =
  let b = restrict ~has_field:i b in
  { bottom with
    blocks = Tagm.map ( Intm.map ( set_a i locs)) b.blocks;
    expr = b.expr;
  }

let set_field i v b = sets_field i (Locs.singleton v) b


let get_field i b =
  Tagm.fold
    (fun _ b acc ->
       Intm.fold
         (fun s a acc ->
            if s > i
            then Locs.union acc a.(i)
            else acc
         ) b acc
    ) b.blocks Locs.empty

let sizes ~tag { blocks; _ } =
  let a = Tagm.find tag blocks in
  Intm.fold (fun s _ i -> Int.join (Int.singleton s) i) a bottom

let make_basic tag size arr =
  { bottom with
    blocks = Tagm.singleton tag ( Intm.singleton size arr )
  }

let fold_field f i b acc env =
  Tagm.fold
    (fun _ b acc ->
       Intm.fold
         (fun s a acc ->
            if s > i
            then Access.fold__locs f a.(i) acc env
            else acc) b acc
    ) b.blocks acc

let on_field f i b env =
  fold_field (Access.fold_to_on f env) i b Access.on_first_acc env
