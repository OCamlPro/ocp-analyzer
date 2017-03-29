open Common_types
open Lambda
open Asttypes
open Ident
open Xlambda

let xlet xe_id xe_lam xe_in =
  Xlet { xe_id; xe_lam; xe_in; xe_kind = Alias; xe_floc = None; xe_loc = None }

let last_id = ref (-1)
let mk_id s =
  incr last_id;
  ( "", { stamp = !last_id; name = s; flags = 0 } )

let ret_id = mk_id "return"

let tend = Xend ret_id

let tconst_int i = Xconst ( Const_base ( Const_int i))
let tadd a b = Xprim ( XPaddint, [a;b])

(* simple test *)
let t1 =
  xlet ret_id ( tconst_int 42) tend

let a = mk_id "a"
let b = mk_id "b"

(* addition test *)
let t2 =
  xlet a ( tconst_int 1605)
    ( xlet b ( tconst_int 1666)
        ( xlet ret_id ( tadd a b) tend )
    )


open Xlambda_interpret

let nofuns = Hashtbl.create 1

let () =
  assert ( fst ( xlambda nofuns env_empty t1) = Int 42);
  assert ( fst ( xlambda nofuns env_empty t2) = Int (1605+1666))

let () =
  Format.printf "t2 : %a@." Print_xlambda.xlambda t2
