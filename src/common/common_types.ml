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

module Id =
struct
  open Ident
  type t = Ident.t
  let compare = compare
  let name x = Some x.name
  let to_string x = Printf.sprintf "%s/%d" x.name x.stamp
  let output o x = Printf.fprintf o "%s/%d" x.name x.stamp
  let print pp x =
    if x.name = "()"
    then Format.fprintf pp "()"
    else print pp x
  let idref = ref 0
  let create ?(name="$$") () =
    decr idref;
    { stamp = !idref; name = ( name ); flags = 0; }
end

let builtin id =
  let s = id.Ident.stamp in
  s > 0 && s < 1000

module F = struct
  module FId = MakeId(struct end)
  type t =
    { id: FId.t;
      important: bool;
      debug_info: Location.t option;
    }

  let create ?name () =
    { id = FId.create ?name ();
      important = true; (* Stick to the previous "everything is important" *)
      debug_info = None;
    }

  let equal {id=i1; _} {id=i2; _} = FId.equal i1 i2

  let compare {id=i1; _} {id=i2; _} = FId.compare i1 i2

  let hash {id; _} = FId.hash id

  let name {id; _} = FId.name id

  let to_string {id; _} = FId.to_string id

  let output fd {id; _} = FId.output fd id

  let print ppf {id; _} = FId.print ppf id

  let create_function ?name ~important ~debug_info =
    { id = FId.create ?name ();
      important;
      debug_info;
    }

  let print_function_info ppf f =
    let print_debug_info ppf = function
      | None -> Format.fprintf ppf "(No debug info)"
      | Some loc ->
        (* Location.print_loc isn't exactly what we want,
           so use lower level stuff *)
        let (file, line, start_char) = Location.(get_pos_info loc.loc_start) in
        let (_, _, end_char) = Location.(get_pos_info loc.loc_end) in
        Format.fprintf ppf "File \"%s\", line %d, characters %d-%d"
          file line start_char end_char
    in
    if f.important
    then
      Format.fprintf ppf
        "Function %a: %a@\n"
        FId.print f.id
        print_debug_info f.debug_info
    else
      Format.fprintf ppf
        "Function %a (Currified function)@\n"
        FId.print f.id

  let is_important {important; _} = important
  let n = 2
end

type id = Id.t

type xid = string * id
(* a xlambda id is a module * an id *)
(* empty module means builtin or special *)
(* "Module", { 0; "#self"; 0 } means a global module *)

module XId =
struct
  open Ident
  type t = xid
  let compare = compare
  let name (m,x) = Some ( m ^ x.name )
  let to_string (m,x) = Printf.sprintf "%s.%s/%d" m x.name x.stamp
  let output o (m,x) = Printf.fprintf o "%s.%s/%d" m x.name x.stamp
  let print pp (m,x) = Format.fprintf pp "%s.%s/%d" m x.name x.stamp
  let print_simple pp (_,x) = Id.print pp x
  let idref = ref 0
  let stamp (_,x) = x.stamp
  let create ?(name="") () =
    decr idref;
    ( "", { stamp = !idref; name = ( "$$" ^ name ); flags = 0; } )
  let dual (s,i) = ("_"^s,i)
end

let fun_xid = XId.create ()
let ret_xid = XId.create ()
let exn_xid = XId.create ()
let arg_xid = XId.create ()


type comparison = Lambda.comparison
and array_kind = Lambda.array_kind
and boxed_integer = Lambda.boxed_integer
and bigarray_kind = Lambda.bigarray_kind
and bigarray_layout = Lambda.bigarray_layout
and compile_time_constant = Lambda.compile_time_constant
and direction_flag = Asttypes.direction_flag


type primitive =
  | XPbuiltin
  (* Operations on heap blocks *)
  | XPfield of int
  | XPsetfield of int * bool
  | XPfloatfield of int
  | XPsetfloatfield of int
  | XPduprecord of Types.record_representation * int
  (* Boolean operations *)
  | XPnot
  (* Integer operations *)
  | XPnegint | XPaddint | XPsubint | XPmulint | XPdivint | XPmodint
  | XPandint | XPorint | XPxorint
  | XPlslint | XPlsrint | XPasrint
  | XPintcomp of comparison
  | XPoffsetint of int
  | XPoffsetref of int
  (* Float operations *)
  | XPintoffloat | XPfloatofint
  | XPnegfloat | XPabsfloat
  | XPaddfloat | XPsubfloat | XPmulfloat | XPdivfloat
  | XPfloatcomp of comparison
  (* String operations *)
  | XPstringlength | XPstringrefu | XPstringsetu | XPstringrefs | XPstringsets
  (* Array operations *)
  | XParraylength of array_kind
  | XParrayrefu of array_kind
  | XParraysetu of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | XPisint
  (* Test if the (integer) argument is outside an interval *)
  | XPisout
  (* Bitvect operations *)
  | XPbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | XPbintofint of boxed_integer
  | XPintofbint of boxed_integer
  | XPcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | XPnegbint of boxed_integer
  | XPaddbint of boxed_integer
  | XPsubbint of boxed_integer
  | XPmulbint of boxed_integer
  | XPdivbint of boxed_integer
  | XPmodbint of boxed_integer
  | XPandbint of boxed_integer
  | XPorbint of boxed_integer
  | XPxorbint of boxed_integer
  | XPlslbint of boxed_integer
  | XPlsrbint of boxed_integer
  | XPasrbint of boxed_integer
  | XPbintcomp of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | XPbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | XPbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | XPbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | XPstring_load_16 of bool
  | XPstring_load_32 of bool
  | XPstring_load_64 of bool
  | XPstring_set_16 of bool
  | XPstring_set_32 of bool
  | XPstring_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | XPbigstring_load_16 of bool
  | XPbigstring_load_32 of bool
  | XPbigstring_load_64 of bool
  | XPbigstring_set_16 of bool
  | XPbigstring_set_32 of bool
  | XPbigstring_set_64 of bool
  (* Compile time constants *)
  | XPctconst of compile_time_constant
  (* byte swap *)
  | XPbswap16
  | XPbbswap of boxed_integer
  (* function operations *)
  | XPgetfun of F.t
  | XPfunfield of int
  | XPgetarg
  (* raw pointer manipulation *)
  | XPint_as_pointer
  
type allocator =
  | XPmakearray of array_kind
  | XPmakeblock of int * Asttypes.mutable_flag
  | XPfun of F.t

module AllocationId = MakeId(struct end)

type hinfo =
  | Var of xid
  | Const of Lambda.structured_constant
  | Prim of primitive * xid list
  | Alloc of AllocationId.t * allocator * xid list
  | Constraint of constr
  | App_prep of xid * xid (* function, argument *)
  | App | App_return | App_exn
  | Return of xid | Retexn of xid (* the function exit *)
  | EndLet of xid list (* List of variables going out of scope *)
  | Lazyforce of xid
  | Ccall of Primitive.description * xid list
  | Send of xid * xid
and constr =
  | Ccp of int
  | Ctag of int
  | Cbool of bool
  | Ctype of Type_description.typ_desc
  | Cstring of string
  | Cnotstring of string list

let ppf = Format.std_formatter
