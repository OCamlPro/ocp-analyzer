open Pervasives

let x = ref None

let () =
  for i = 0 to 10 do
    x := Some i;
    ( match Some i with
      | Some a -> assert (a = 0)
      | None -> assert false );
    ()
  done
(* let () = *)
(*   for i = 0 to 10 do *)
(*     assert (i = 0) *)
(*   done *)
