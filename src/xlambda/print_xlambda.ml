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

open Format
open Common_types
open Xlambda

let boxed_integer_mark name bi =
  let open Lambda in
  match bi with
  | Pnativeint -> Printf.sprintf "Nativeint.%s" name
  | Pint32 -> Printf.sprintf "Int32.%s" name
  | Pint64 -> Printf.sprintf "Int64.%s" name

let boxed_integer_name n =
  let open Lambda in match n with
  | Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let print_boxed_integer name ppf bi =
  fprintf ppf "%s" (boxed_integer_mark name bi)

let print_boxed_integer_conversion ppf bi1 bi2 =
  fprintf ppf "%s_of_%s" (boxed_integer_name bi2) (boxed_integer_name bi1)

let record_rep ppf r =
  match r with
  | Types.Record_regular -> fprintf ppf "regular"
  | Types.Record_float -> fprintf ppf "float"

let print_bigarray name unsafe kind ppf layout =
  let open Lambda in
  fprintf ppf "Bigarray.%s[%s,%s]"
    (if unsafe then "unsafe_"^ name else name)
    (match kind with
     | Pbigarray_unknown -> "generic"
     | Pbigarray_float32 -> "float32"
     | Pbigarray_float64 -> "float64"
     | Pbigarray_sint8 -> "sint8"
     | Pbigarray_uint8 -> "uint8"
     | Pbigarray_sint16 -> "sint16"
     | Pbigarray_uint16 -> "uint16"
     | Pbigarray_int32 -> "int32"
     | Pbigarray_int64 -> "int64"
     | Pbigarray_caml_int -> "camlint"
     | Pbigarray_native_int -> "nativeint"
     | Pbigarray_complex32 -> "complex32"
     | Pbigarray_complex64 -> "complex64")
    (match layout with
    |  Pbigarray_unknown_layout -> "unknown"
     | Pbigarray_c_layout -> "C"
     | Pbigarray_fortran_layout -> "Fortran")

let id_list ppf ids = List.iter (fun l -> fprintf ppf "@ %a" XId.print_simple l) ids

let print_loc ppf = function
  | None -> fprintf ppf ""
  | Some l -> fprintf ppf "@[(%a)@]" Location.print_loc l

let rec xlambda ppf = function
  | Xlet t -> xlet ppf t
  | Xrec t -> xrec ppf t
  | Xend xid -> tend ppf xid

and xlet ppf tl =
  let rec aux ppf = function
    | Xlet t -> one ppf t
    | Xrec t -> xrec ppf t
    | Xend xid -> tend ppf xid
  and one ppf { xe_id; xe_lam; xe_kind; xe_in; xe_loc;_ } =
    fprintf ppf "@[@[<2>let %a%a@ =@ @[<2>%a@]@] in@ %a@]"
      XId.print_simple xe_id
      print_loc xe_loc
      xcontrol xe_lam
      aux xe_in
  in
  fprintf ppf "@[<2>%a@]" one tl

and xrec ppf { xr_decls; xr_in } =
  fprintf ppf "@[@[<2>lexrec@ ";
  List.iter (fun ( i, p, args) ->
      fprintf ppf "@[%a@ =@ %a@ %a@]"
        XId.print_simple i
        allocator p
        id_list args
    ) xr_decls;
  fprintf ppf "@] in@ %a@]"
    xlambda xr_in

and tend ppf xid = fprintf ppf "%a" XId.print_simple xid

and xcontrol ppf = function
  | Xvar id -> XId.print_simple ppf id
  | Xconst c -> Printlambda.structured_constant ppf c
  | Xapply (f, arg) ->
    fprintf ppf "apply@ %a@ %a" XId.print_simple f XId.print_simple arg
  | Xprim (p,args) ->
    fprintf ppf "%a%a" primitive p id_list args
  | Xalloc (p,args) ->
    fprintf ppf "%a%a" allocator p id_list args
  | Xswitch (case, sw) ->
    let switch ppf sw =
      let spc = ref false in
      List.iter
        (fun (n, l) ->
           if !spc then fprintf ppf "@ | " else spc := true;
           fprintf ppf "@[<hv 1>%i ->@ %a@]" n xlambda l)
        sw.x_consts;
      List.iter
        (fun (n, l) ->
           if !spc then fprintf ppf "@ | " else spc := true;
           fprintf ppf "@[<hv 1>%ip ->@ %a@]" n xlambda l)
        sw.x_blocks ;
      begin match sw.x_failaction with
        | None  -> ()
        | Some l ->
          if !spc then fprintf ppf "@ | " else spc := true;
          fprintf ppf "@[<hv 1>_ ->@ %a@]" xlambda l
      end in
    fprintf ppf
      "@[<1>switch %a with@ %a@]" XId.print_simple case switch sw
  | Xstringswitch ( sid, l, default ) ->
    let switch_print ppf l =
      List.iter (fun (s, xlam) ->
          fprintf ppf "@ | @[<hv 1>%s ->@ %a@]" s xlambda xlam)
        l
    in
    let default_print ppf = function
      | None -> ()
      | Some xlam -> fprintf ppf "@ | @[<hv 1>_ ->@ %a@]" xlambda xlam
    in  
    fprintf ppf
      "@[<1>stringswitch %a with@ %a%a@]"
      XId.print_simple sid
      switch_print l
      default_print default
  | Xstaticraise (i, ids) ->
    fprintf ppf "staticraise %i %a" i id_list ids
  | Xstaticcatch (body, (n, ids), handler) ->
    fprintf ppf "@[<2>catch@ %a@ with@ %i@ (%a)@ -> %a@]"
      xlambda body n id_list ids xlambda handler
  | Xraise (_,id) ->
    fprintf ppf "raise@ %a" XId.print_simple id
  | Xtrywith (body, id, handler) ->
    fprintf ppf "@[<2>try@ %a@ with@ %a@ -> %a@]"
      xlambda body XId.print_simple id xlambda handler
  | Xifthenelse (cond, ifso, ifnot) ->
    fprintf ppf "@[<2>if@ %a@ then@ %a@ else@ %a@]"
      XId.print_simple cond xlambda ifso xlambda ifnot
  | Xwhile (cond, body) ->
    fprintf ppf "@[<2>while@ %a@ do@ %a@ done@]"
      xlambda cond xlambda body
  | Xfor (i,start,stop,dir,body) ->
    fprintf ppf "@[<2>for@ %a@ =@ %a@ %s@ %a do@ %a@ done@]"
      XId.print_simple i
      XId.print_simple start
      (if dir = Asttypes.Upto then "to" else "downto" )
      XId.print_simple stop
      xlambda body
  | Xlazyforce id ->
    fprintf ppf "lazyforce@ %a" XId.print_simple id
  | Xccall (p,l) ->
    begin
      match l with
      | hd::tl ->
        fprintf ppf "{%s}@ (%a%a)"
          p.Primitive.prim_name
          XId.print_simple hd
          (fun ppf -> List.iter (fprintf ppf ",@ %a" XId.print_simple)) tl
      | [] -> assert false
    end
  | Xsend (_,o,m) ->
    fprintf ppf "%a#%a"
      XId.print_simple o XId.print_simple m

and allocator ppf p =
  let open Asttypes in
  match p with
  | XPfun f -> fprintf ppf "fun %a" F.print f
  | XPmakeblock (tag, Immutable) ->
    fprintf ppf "makeblock(%i)" tag
  | XPmakeblock (tag, Mutable) ->
    fprintf ppf "makemutable(%i)" tag
  | XPmakearray _ -> fprintf ppf "makearray "

and primitive ppf p =
  let open Lambda in
  match p with
  | XPgetfun f -> fprintf ppf "getfun %a" F.print f
  | XPfunfield i -> fprintf ppf "funfield %i" i
  | XPgetarg -> fprintf ppf "getarg"
  | XPbuiltin -> fprintf ppf "builtin"

  | XPnegint -> fprintf ppf "~"
  | XPaddint -> fprintf ppf "+"
  | XPsubint -> fprintf ppf "-"
  | XPmulint -> fprintf ppf "*"
  | XPdivint -> fprintf ppf "/"
  | XPmodint -> fprintf ppf "mod"
  | XPandint -> fprintf ppf "and"
  | XPorint -> fprintf ppf "or"
  | XPxorint -> fprintf ppf "xor"
  | XPlslint -> fprintf ppf "lsl"
  | XPlsrint -> fprintf ppf "lsr"
  | XPasrint -> fprintf ppf "asr"

  | XPintcomp(Ceq) -> fprintf ppf "=="
  | XPintcomp(Cneq) -> fprintf ppf "!="
  | XPintcomp(Clt) -> fprintf ppf "<"
  | XPintcomp(Cle) -> fprintf ppf "<="
  | XPintcomp(Cgt) -> fprintf ppf ">"
  | XPintcomp(Cge) -> fprintf ppf ">="
  | XPintoffloat -> fprintf ppf "int_of_float"
  | XPfloatofint -> fprintf ppf "float_of_int"
  | XPnegfloat -> fprintf ppf "~."
  | XPabsfloat -> fprintf ppf "abs."
  | XPaddfloat -> fprintf ppf "+."
  | XPsubfloat -> fprintf ppf "-."
  | XPmulfloat -> fprintf ppf "*."
  | XPdivfloat -> fprintf ppf "/."

  | XPfloatcomp(Ceq) -> fprintf ppf "==."
  | XPfloatcomp(Cneq) -> fprintf ppf "!=."
  | XPfloatcomp(Clt) -> fprintf ppf "<."
  | XPfloatcomp(Cle) -> fprintf ppf "<=."
  | XPfloatcomp(Cgt) -> fprintf ppf ">."
  | XPfloatcomp(Cge) -> fprintf ppf ">=."

  | XPstringlength -> fprintf ppf "string.length"
  | XPstringrefu -> fprintf ppf "string.unsafe_get"
  | XPstringsetu -> fprintf ppf "string.unsafe_set"
  | XPstringrefs -> fprintf ppf "string.get"
  | XPstringsets -> fprintf ppf "string.set"

  | XParraylength _ -> fprintf ppf "array.length"
  | XParrayrefu _ -> fprintf ppf "array.unsafe_get"
  | XParraysetu _ -> fprintf ppf "array.unsafe_set"

  | XPisint -> fprintf ppf "isint"
  | XPisout -> fprintf ppf "isout"
  | XPbittest -> fprintf ppf "testbit"

  | XPstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get16"
     else fprintf ppf "string.get16"
  | XPstring_load_32(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get32"
     else fprintf ppf "string.get32"
  | XPstring_load_64(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get64"
     else fprintf ppf "string.get64"
  | XPstring_set_16(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set16"
     else fprintf ppf "string.set16"
  | XPstring_set_32(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set32"
     else fprintf ppf "string.set32"
  | XPstring_set_64(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set64"
     else fprintf ppf "string.set64"
  | XPbigstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get16"
     else fprintf ppf "bigarray.array1.get16"
  | XPbigstring_load_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get32"
     else fprintf ppf "bigarray.array1.get32"
  | XPbigstring_load_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get64"
     else fprintf ppf "bigarray.array1.get64"
  | XPbigstring_set_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set16"
     else fprintf ppf "bigarray.array1.set16"
  | XPbigstring_set_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set32"
     else fprintf ppf "bigarray.array1.set32"
  | XPbigstring_set_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set64"
     else fprintf ppf "bigarray.array1.set64"

  | XPbswap16 -> fprintf ppf "bswap16"
  | XPbbswap(bi) -> print_boxed_integer "bswap" ppf bi

  | XPfield n -> fprintf ppf "field %i" n
  | XPsetfield(n, ptr) ->
      let instr = if ptr then "setfield_ptr " else "setfield_imm " in
      fprintf ppf "%s%i" instr n
  | XPfloatfield n -> fprintf ppf "floatfield %i" n
  | XPsetfloatfield n -> fprintf ppf "setfloatfield %i" n
  | XPduprecord (rep, size) -> fprintf ppf "duprecord %a %i" record_rep rep size

  | XPnot -> fprintf ppf "not"
  | XPoffsetint n -> fprintf ppf "%i+" n
  | XPoffsetref n -> fprintf ppf "+:=%i"n
  | XPbintofint bi -> print_boxed_integer "of_int" ppf bi
  | XPintofbint bi -> print_boxed_integer "to_int" ppf bi
  | XPcvtbint (bi1, bi2) -> print_boxed_integer_conversion ppf bi1 bi2
  | XPnegbint bi -> print_boxed_integer "neg" ppf bi
  | XPaddbint bi -> print_boxed_integer "add" ppf bi
  | XPsubbint bi -> print_boxed_integer "sub" ppf bi
  | XPmulbint bi -> print_boxed_integer "mul" ppf bi
  | XPdivbint bi -> print_boxed_integer "div" ppf bi
  | XPmodbint bi -> print_boxed_integer "mod" ppf bi
  | XPandbint bi -> print_boxed_integer "and" ppf bi
  | XPorbint bi -> print_boxed_integer "or" ppf bi
  | XPxorbint bi -> print_boxed_integer "xor" ppf bi
  | XPlslbint bi -> print_boxed_integer "lsl" ppf bi
  | XPlsrbint bi -> print_boxed_integer "lsr" ppf bi
  | XPasrbint bi -> print_boxed_integer "asr" ppf bi
  | XPbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi
  | XPbintcomp(bi, Cneq) -> print_boxed_integer "!=" ppf bi
  | XPbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi
  | XPbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi
  | XPbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi
  | XPbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi

  | XPbigarrayref(unsafe, n, kind, layout) ->
      print_bigarray "get" unsafe kind ppf layout
  | XPbigarrayset(unsafe, n, kind, layout) ->
      print_bigarray "set" unsafe kind ppf layout
  | XPbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n

  | XPctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin" in
     fprintf ppf "sys.constant_%s" const_name
  | XPint_as_pointer ->
    fprintf ppf "print_as_pointer"
