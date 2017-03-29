open Pervasives

let () =
  assert( ~- 1 = -1 );
  assert( ~+ 1 = 1 );
  assert( succ 1 = 2 );
  assert( pred 1 = 0 );
  assert( 1 + 1 = 2 );
  assert( 1 - 1 = 0 );
  (* assert( 1 * 2 = 2 ); *)
  (* assert( 1 / 1 = 1 ); *)
  (* assert( 3 mod 2 = 1 ); *)
  assert( abs (-1) = 1 );
  (* assert( -1 land 3 = 3 ); *)
  (* assert( -1 lor 3 = -1 ); *)
  (* assert( -1 lxor 3 = -4 ); *)
  (* assert( lnot (-1) = 0 ); *)
  (* assert( 1 lsl 2 = 4 ); *)
  (* assert( 1 lsr 1 = 0 ); *)
  (* assert( 1 asr 1 = 0 ); *)
  ()

let () =
  assert( 1 = 1 );
  assert( not( 1 = 2 ) );
  assert( 1 <> 2 );
  assert( not( 1 <> 1 ) );
  assert( 1 == 1 );
  assert( not( 1 == 2 ) );
  assert( 1 != 2 );
  assert( not( 1 != 1 ) );
  assert( 1 < 2 );
  assert( not( 2 < 2 ) );
  assert( 2 > 1 );
  assert( not( 2 > 2 ) );
  assert( 2 <= 2 );
  assert( not( 3 <= 2 ) );
  assert( 2 >= 2 );
  assert( not( 2 >= 3 ) );
  ()
