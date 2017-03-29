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

type edge =
  (* The root: variable name *)
  | Varname of xid
  (* Denotes an allocation point *)
  | Allocation of (* loc * *) allocator
  (* Block accessing *)
  | BlockSize of int (* necessary ? *)
  | Field of int
  (* Array accessing *)
  | ArrayElements
  | ArraySize
  (* Functions *)
  | FunField of int
  (* (\* Simple values *\) *)
(* | Integer of Int_interv.t *)
(* | ConstPointer of Cps.t *)
(* | Constant of Constants.t *)

module Edge =
struct
  type t = edge

  let compare (a:t) b = Pervasives.compare a b

  let print ppf = function
    | Varname x -> Format.fprintf ppf "$%a" XId.print x
    | Allocation ((* l, *)alloc) ->
      begin
        match alloc with
        | XPmakearray k ->
          let open Lambda in
          Format.fprintf ppf "[|%c|]"  (* "[|%a:%c|]" *)
                                                         (* Loc.print l *) (match k with
              | Pgenarray -> 'g'
              | Paddrarray -> 'a'
              | Pintarray -> 'i'
              | Pfloatarray -> 'f')
        | XPmakeblock (tag,mut) ->
          Format.fprintf ppf "[(%d%s)]" (* "[(%d%s):%a]" *)
            tag
            ( let open Asttypes in
              match mut with | Mutable -> "m" | Immutable -> "" )
        (* Loc.print l *)
        | XPfun fid ->
          Format.fprintf ppf "(fun->%a)" (* "(%a->%a)" *)
          (* Loc.print l *) F.print fid
      end
    | BlockSize i -> Format.fprintf ppf "|%d|" i
    | Field i -> Format.fprintf ppf "[%d]" i
    | ArrayElements -> Format.pp_print_string ppf ".()"
    | ArraySize -> Format.pp_print_string ppf "|a|"
    | FunField i -> Format.fprintf ppf "(%d)" i
  (* | Integer ii -> Int_interv.print ppf ii *)
  (* | ConstPointer cp -> Cps.print ppf cp *)
  (* | Constant c -> Constants.print ppf c *)

  let equal a b = match a, b with
    | Varname x, Varname y -> 0 = XId.compare x y
    | Allocation a, Allocation b -> a = b
    | BlockSize a, BlockSize b
    | Field a, Field b
    | FunField a, FunField b -> a = b
    | ArrayElements, ArrayElements
    | ArraySize, ArraySize -> true
    | _,_ -> false

  let hash = Hashtbl.hash

end

module Terminal =
struct
  type t =
    {
      integers : Int_interv.t;
      const_pointers: Cps.t;
      constants : Constants.t;
    }

  let compare (a:t) b = compare a b
  let print ppf x =
    Format.fprintf ppf
      "[[@[Int : %a; Cps: %a; Consts: %a; @]]]"
      Int_interv.print x.integers
      Cps.print x.const_pointers
      Constants.print x.constants

  let join a b =
    {
      integers = Int_interv.join a.integers b.integers;
      const_pointers = Cps.join a.const_pointers b.const_pointers;
      constants = Constants.join a.constants b.constants;
    }

  let bottom =
    {
      integers = Int_interv.bottom;
      const_pointers = Cps.bottom;
      constants = Constants.bottom;
    }

end

module Em = CurryMap.Make (Edge)
module Es = CurrySet.Make (Edge)

module Diff = Difftrie.Make (Edge)
module type DiffFolder = Difftrie.Folder with type edge := edge

type path = edge list

(* module Paths = Trie.Make (Edge) *)

type tree = Node of ( edge * tree ) list * int option * Terminal.t option | Back of edge * int

module Im =
  CurryMap.MakePrint (struct
    type t = int
    let compare (a:t) b = compare a b
    let print = Format.pp_print_int
  end)

module type Ident =
sig
  type t
  include CurryMap.OrderedType with type t := t
  include CurrySet.OrderedType with type t := t
end
