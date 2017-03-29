external raise : exn -> 'a = "%raise"

exception Exn of string

let f () =
  raise (Exn "Exn")

let () =
  try
    f ()
  with
  | Exn "Exn" -> ()
  | _ -> assert false

(* Ideal result: No exception
   Expected result: Assert_failure *)
