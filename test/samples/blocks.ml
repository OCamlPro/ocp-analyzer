external ( = ) : 'a -> 'a -> bool = "%equal"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ignore : 'a -> unit = "%ignore"
external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"
type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"

let () =
  assert( fst (1,2) = 1 );
  assert( snd (1,2) = 2 );
  ()


let () =
  let r = ref 0 in
  assert( !r = 0 );
  r := 1;
  assert( !r = 1 );
  incr r;
  assert( !r = 2 );
  decr r;
  assert( !r = 1 );
  ()

let () =
  let r = ref 0 in
  ignore( r == r ); (* forbid promotion to variable *)
  assert( !r = 0 );
  r := 1;
  assert( !r = 1 );
  incr r;
  assert( !r = 2 );
  decr r;
  assert( !r = 1 );
  ()
