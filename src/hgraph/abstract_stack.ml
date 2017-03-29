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

let equal_option eq v1 v2 =
  match v1,v2 with
  | None, None -> true
  | Some v1, Some v2 -> eq v1 v2
  | _ -> false

let compare_option comp v1 v2 =
  match v1,v2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some v1, Some v2 -> comp v1 v2

let hash_option hash = function
  | None -> 0
  | Some v -> hash v

let stack_unknown_prefix = Concat [Star Any; Start]

module OneLevel (T:OrderedHashedType) : Stack with type elt = T.t =
struct
  type elt = T.t
  type t = elt option

  let empty = None

  let push _ e = Some e
  let equal = equal_option T.equal
  let compare = compare_option T.compare
  let hash = hash_option T.hash
  let print ppf = function
    | None -> Format.fprintf ppf "empty stack"
    | Some s -> T.print ppf s

  let to_stack_expr = function
    | None -> Start
    | Some s -> Concat [Elt s; stack_unknown_prefix]
end

module TwoLevels (T:OrderedHashedType) : Stack with type elt = T.t =
struct
  type elt = T.t
  type t =
    { context : elt option;
      current : elt option }

  let empty = { context = None; current = None }

  let push { current; _ } e =
    { context = current;
      current = Some e }

  let equal t1 t2 =
    equal_option T.equal t1.current t2.current &&
    equal_option T.equal t1.context t2.context

  let compare t1 t2 =
    let c = compare_option T.compare t1.current t2.current in
    if c = 0
    then compare_option T.compare t1.context t2.context
    else c

  let hash t =
    Hashtbl.hash (hash_option T.hash t.current,
                  hash_option T.hash t.context)

  let print ppf = function
    | { current = None; _ } -> Format.fprintf ppf "empty stack"
    | { context = None; current = Some s } ->
      Format.fprintf ppf "(%a)" T.print s
    | { context = Some s2; current = Some s1 } ->
      Format.fprintf ppf "(%a::%a)" T.print s1 T.print s2

  let to_stack_expr t =
    match t.current, t.context with
    | None, _ -> Start
    | Some cur, None ->
      Concat [Elt cur; Start]
    | Some cur, Some ctx ->
      Concat [Elt cur; Elt ctx; stack_unknown_prefix]
end

module Local (Top:Stack) : Stack with type elt = Top.elt =
struct

  module TopSet = Set.Make(Top)

  type elt = Top.elt

  type t =
    { env : TopSet.t;
      top : Top.t }

  let empty = { env = TopSet.singleton Top.empty; top = Top.empty }

  let push { env; top = stack } elt =
    let top = Top.push stack elt in
    let env = TopSet.add top env in
    { env; top }

  let equal s1 s2 =
    Top.equal s1.top s2.top &&
    TopSet.equal s1.env s2.env

  let compare s1 s2 =
    let c = Top.compare s1.top s2.top in
    if c = 0
    then TopSet.compare s1.env s2.env
    else c

  let hash { env; top } =
    Hashtbl.hash (Top.hash top, TopSet.cardinal env)

  let print ppf { top; _ } = Top.print ppf top

  let to_stack_expr { top; _ } =
    (* I'm not sure why Top is an argument and not a direct reference
       to the above-defined module, but it means that we can't use
       the env field here *)
    Top.to_stack_expr top
end

module Leveled (T:LeveledFunction) =
struct

  assert(T.n>=1);;

  type elt = T.t

  type level =
    | Important
    | Not_important

  type t =
    { depth : int;
      stack : (level * elt) list }

  let empty = { depth = 0; stack = [] }

  let rec keep_n n = function
    | [] -> []
    | (Not_important, _) as h :: q ->
      h :: (keep_n n q)
    | (Important, _) as h :: q ->
      if n = 0
      then []
      else h :: (keep_n (n-1) q)

  let push { depth; stack } elt =
    let important = T.is_important elt in
    if important
    then
      if depth = T.n
      then
        { depth = T.n;
          stack = keep_n T.n ((Important, elt) :: stack) }
      else
        { depth = depth + 1;
          stack = (Important, elt) :: stack }
    else
      { depth = depth;
        stack = (Not_important, elt) :: stack }

  let equal st1 st2 =
    st1.depth = st2.depth &&
    List.for_all2 (fun (_, e1) (_, e2) -> T.equal e1 e2) st1.stack st2.stack

  let rec compare_list l1 l2 = match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | (_,e1)::t1, (_,e2)::t2 ->
      let c = T.compare e1 e2 in
      if c <> 0
      then c
      else compare_list t1 t2

  let compare st1 st2 =
    compare_list st1.stack st2.stack

  let hash { depth; stack } =
    match stack with
    | [] -> 0
    | (_,h)::_ -> Hashtbl.hash (depth, T.hash h)

  let print ppf { depth; stack } =
    let rec aux depth = function
      | [] -> ()
      | (Not_important, _) :: t -> aux depth t
      | (Important, v) :: t ->
        T.print ppf v;
        if depth > 1
        then Format.fprintf ppf " :: ";
        aux (depth-1) t
    in
    aux depth stack

  let to_stack_expr { depth; stack } =
    let stack_bottom =
      if depth < T.n
      then Start
      else stack_unknown_prefix
    in
    let rec mk_list = function
      | [] -> [stack_bottom]
      | (Not_important, _) :: q -> mk_list q
      | (Important, v) :: q -> (Elt v) :: (mk_list q)
    in
    Concat (mk_list stack)

end
