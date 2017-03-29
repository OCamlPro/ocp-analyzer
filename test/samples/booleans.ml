open Pervasives

let () =
  assert true;
  assert( not false );
  assert( true && true );
  assert(not ( true && false ) );
  assert(not ( false && true ) );
  assert(not ( false && false ) );
  assert( true || true );
  assert( true || false );
  assert( false || true );
  assert(not ( false || false ));
  ()
