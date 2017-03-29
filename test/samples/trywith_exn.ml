external raise : exn -> 'a = "%raise"

exception Exn
exception Exn'

let f () =
  raise Exn

let () =
  try
    f ()
  with
  | Exn' -> ()
  | _ -> assert false

(* Ideal result: Assert_failure
   Actual result: Assert_failure
*)
