external raise : exn -> 'a = "%raise"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"

exception Exn

let f () =
  raise Exn

let () =
  try
    f ()
  with
  | Exn -> ()
  | _ -> assert false

(* Ideal result: No exception
   Actual result: No exception
   Required feature: correct handling of structural equality *)

exception Exn2 of string

let g () =
  raise (Exn2 "")

let () =
  let x =
    try
      g ()
    with
    | Exn2 "" -> 2
    | Exn2 _ -> 1
    | _ -> 0
  in
  assert (x >= 1)

(* Ideal result: No exception
   Actual result: Any exception (but only from the Ccall caml_string_notequal)
   Required feature: Precise handling of Ccall primitives *)
