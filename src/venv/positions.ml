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

module Pos = struct
  type t = int
  let equal (a:t) b = a = b
  let compare (a:t) b = compare a b
  let hash (a:t) = Hashtbl.hash a
  let to_string = string_of_int
  let output o a = output_string o (to_string a)
  let print ppf a = Format.fprintf ppf ".%d" a
  let create = let v = ref 0 in fun () -> incr v; !v
end
    
