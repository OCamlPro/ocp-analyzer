
let to_print = ref []

let arg_parser =
  let open Arg in
  [
    ( "-open",
      String Mk_lambda.open_module,
      "Add an implicitly opened module" );
    ( "-print",
      String (fun s -> to_print := s :: !to_print),
      "Print xlambda of the module" )
  ]

let lamq = Queue.create ()

let () = Arg.parse
    arg_parser
    (fun sourcefile ->
       let lm = Mk_lambda.mk_lambda Format.std_formatter sourcefile in
       Queue.add lm lamq
    )
    "please specify your .ml or .cmt files, order matters"

let lambdas =
  Array.init ( Queue.length lamq ) (fun _ -> Queue.pop lamq)

let () =  print_endline "Got lambdas !"


let funs : ( Common_types.F.t, Xlambda.xlambda ) Hashtbl.t = Hashtbl.create 1024

let xlambdas =
  Array.map
    (fun ( lam, modname ) ->
       let tl = Mk_xlambda.lambda_to_xlambda ~modname ~funs lam in
       if List.mem modname !to_print
       then Format.printf "%s:@ %a@." modname
           Print_xlambda.xlambda tl;
       tl)
    lambdas

let () =  print_endline "Got Xlambdas !"

let _ = Array.fold_left (fun (_,env) -> Xlambda_interpret.xlambda funs env )
    (Xlambda_interpret.val_unit, Xlambda_interpret.env_empty) xlambdas
