open Pervasives

(* open Myper *)

let () =
  (* assert( compare 0 0 = 0 ); *)
  (* assert( compare 0 1 < 0 ); *)
  (* assert( compare 1 0 > 0 ); *)
  ()


let () =
  (* assert( ~- min_int = min_int ); *)
  (* assert( ~- max_int = min_int + 1 ); *)
  ()


let () =
  (* let a = "a" in *)
  (* let b = "b" in *)
  (* assert( a == a ); *)
  (* assert( a != b ); *)
  (* assert( "a" = "a" ); *)
  (* assert( "a" <> "b" ); *)
  (* assert( "a" ^ "b" = "ab" ); *)
  (* assert( "a" ^ "c" <> "ab" ); *)
  ()

let () =
  (* assert( ignore 1 = () ); *)
  (* assert( string_of_bool true = "true" ); *)
  ()




type tt =
  | A | B | C | D
  | E of int | F of int

exception Exn1
exception Exn2

let () =

  let x =
    match A with
    | A -> 1
    | _ -> 2 in
  assert(x = 1);

  let x =
    match B with
    | A -> 1
    | C -> 2
    | _ -> 3 in
  assert(x = 3);

  let x =
    match E 4 with
    | E x -> x
    | _ -> 0 in
  assert(x = 4);

  let r = ref None in
  for i = 0 to 1 do
    let v =
      if i = 0
      then E 5
      else F 4
    in
    match v with
    | E i -> r := Some i
    | F i -> r := Some (i + 1)
    | _ -> r := Some 0
  done;
  (match !r with
   | Some x ->
     (* TODO: add a primitive to help assert that a node is reachable *)
     (* assert_visited () *)
     assert(x = 5)
   | None ->
     assert false;
     ());

  for i = 0 to 10 do
    let v = match i with
      | 0 -> A
      | 1 -> B
      | 2 -> C
      | 3 -> D
      | 4 -> E 1
      | n -> F n
    in

    match v with
    | A ->
      (match v with
       | A ->
         (* assert_visited () *)
         ()
       | _ -> assert false)
    | E _ ->
      (match v with
       | E _ ->
         (* assert_visited () *)
         ()
       | _ -> assert false)
    (* | F n -> *)
    (*   (match v with *)
    (*    | F m -> *)
    (*      (\* assert_visited () *\) *)
    (*      assert (n = m) *)
    (*    | _ -> raise Exn1) *)
    | _ -> ()

  done;
  (* assert_visited (); *)
  ()
