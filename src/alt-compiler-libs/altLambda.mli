(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The "lambda" intermediate code *)

open Asttypes

type compile_time_constant = Lambda.compile_time_constant =
  | Big_endian
  | Word_size
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin

type 'typedef primitive =
    Pidentity
  | Pignore
  | Prevapply of Location.t
  | Pdirapply of Location.t
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * 'typedef
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int
  | Psetfloatfield of int
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray of array_kind * 'typedef
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of boxed_integer
  | Pmodbint of boxed_integer
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pstring_set_16 of bool
  | Pstring_set_32 of bool
  | Pstring_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer

and comparison = Lambda.comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

and array_kind = Lambda.array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and boxed_integer = Lambda.boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind = Lambda.bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout = Lambda.bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt | Variable
(* Meaning of kinds for let x = e in e':
    Strict: e may have side-effets; always evaluate e first
      (If e is a simple expression, e.g. a variable or constant,
       we may still substitute e'[x/e].)
    Alias: e is pure, we can substitute e'[x/e] if x has 0 or 1 occurrences
      in e'
    StrictOpt: e does not have side-effects, but depend on the store;
      we can discard e if x does not appear in e'
    Variable: the variable x is assigned later in e' *)

type meth_kind = Self | Public | Cached

type shared_code = (int * int) list     (* stack size -> code label *)

type 'a lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of 'a lambda * 'a lambda list * Location.t * 'a
  | Lfunction of function_kind * Ident.t list * 'a lambda
  | Llet of let_kind * Ident.t * 'a lambda * 'a lambda
  | Lletrec of (Ident.t * 'a lambda) list * 'a lambda
  | Lprim of 'a primitive * 'a lambda list * 'a option
  | Lswitch of 'a lambda * 'a lambda_switch
  | Lstaticraise of int * 'a lambda list
  | Lstaticcatch of 'a lambda * (int * Ident.t list) * 'a lambda
  | Ltrywith of 'a lambda * Ident.t * 'a lambda
  | Lifthenelse of 'a lambda * 'a lambda * 'a lambda
  | Lsequence of 'a lambda * 'a lambda
  | Lwhile of 'a lambda * 'a lambda
  | Lfor of Ident.t * 'a lambda * 'a lambda * direction_flag * 'a lambda
  | Lassign of Ident.t * 'a lambda
  | Lsend of meth_kind * 'a lambda * 'a lambda * 'a lambda list * Location.t
  | Levent of 'a lambda * lambda_event
  | Lifused of Ident.t * 'a lambda

and 'a lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * 'a lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * 'a lambda) list;     (* Tag block cases *)
    sw_failaction : 'a lambda option}      (* Action to take if failure *)
and lambda_event = Lambda.lambda_event =
  { lev_loc: Location.t;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.summary }

and lambda_event_kind = Lambda.lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function

val same: 'a lambda -> 'a lambda -> bool
val const_unit: structured_constant
val lambda_unit: 'a lambda
val name_lambda: 'a lambda -> (Ident.t -> 'a lambda) -> 'a lambda
val name_lambda_list: 'a lambda list -> ('a lambda list -> 'a lambda) -> 'a lambda

val iter: ('a lambda -> unit) -> 'a lambda -> unit

val free_variables: 'a lambda -> Lambda.IdentSet.t
val free_methods: 'a lambda -> Lambda.IdentSet.t

val transl_path: Path.t -> 'a lambda
val make_sequence: ('a -> 'b lambda) -> 'a list -> 'b lambda

val subst_lambda: 'a lambda Ident.tbl -> 'a lambda -> 'a lambda
val bind : let_kind -> Ident.t -> 'a lambda -> 'a lambda -> 'a lambda

val commute_comparison : comparison -> comparison
val negate_comparison : comparison -> comparison

(***********************)
(* For static failures *)
(***********************)

(* Get a new static failure ident *)
val next_raise_count : unit -> int


val staticfail : 'a lambda (* Anticipated static failure *)

(* Check anticipated failure, substitute its final value *)
val is_guarded: 'a lambda -> bool
val patch_guarded : 'a lambda -> 'a lambda -> 'a lambda
