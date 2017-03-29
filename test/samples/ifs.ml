open Pervasives

let () =
  (* let x = if true then 1 else 2 in *)
  (* assert( x = 1 ); *)
  (* let x = if false then 1 else 2 in *)
  (* assert( x = 2 ); *)
  (* let x = if true then 1 else assert false in *)
  (* assert( x = 1 ); *)
  (* let x = if false then assert false else 2 in *)
  (* assert( x = 2 ); *)

  for i = 1 to 10 do
    (* if i = 5 then assert(i = 5); *)
    (* if i <> 5 then assert(i <> 5); *) (* cannot be represented by interval *)
    if i > 5 then assert(i > 5);
    (* if i < 5 then assert(i < 5); *) (* need narrowing or smarter widening *)
  done;

  ()
