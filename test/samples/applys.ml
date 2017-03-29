external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
external ( = ) : 'a -> 'a -> bool = "%equal"
external not : bool -> bool = "%boolnot"
external ( + ) : int -> int -> int = "%addint"

let () =
  let f x = x + x in
  assert( 1 |> f = 2 );
  (* assert( 1 |> f |> f = 4 ); *)
  assert( f @@ 1 = 2 );
  (* assert( f @@ f @@ 1 = 4 ); *)
  ()



let () =
  let f x y = x + y in
  let g f x = f x in
  assert( f 1 2 = 3 );
  assert( not (f 1 2 = 4) );
  assert( g f 1 2 = 3 );
  assert( not (g f 1 2 = 4) );
  ()
