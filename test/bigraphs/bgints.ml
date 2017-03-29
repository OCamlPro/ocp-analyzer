open Common_types
open Test_data

let g = H.create ()

let a_id = xid "a"
let b_id = xid "b"
let c_id = xid "c"
let f_id = xid "f"
let closure_id = xid "f"

let n_cst = 1
let m_cst = 42

let func = F.create ~name:"func" ()

let v0 = "v0"
let v1 = "v1"
let v2 = "v2"
let v3 = "v3"
let v4 = "v4"
let v5 = "v5"

let () =
  let vert = [v0;v1;v2;v3;v4;v5] in
  List.iter (fun v -> H.add_vertex g v ()) vert;
  H.add_hedge g "0.1" (Set_int (a_id,n_cst)) ~pred:[|v0|] ~succ:[|v1|];
  H.add_hedge g "1.2" (Set_int (b_id,m_cst)) ~pred:[|v1|] ~succ:[|v2|];
  H.add_hedge g "2.3" (Set_closure (f_id, func, [|a_id|])) ~pred:[|v2|] ~succ:[|v3|];
  H.add_hedge g "3.4" (Prepare_call (f_id, b_id)) ~pred:[|v3|] ~succ:[|v4|];
  H.add_hedge g "4.5" Call ~pred:[|v4|] ~succ:[|v5|]

let g_func = H.create ()

let v_in = "v.in"
let v7 = "v7"
let v8 = "v8"
let v9 = "v9"
let v10 = "v10"
let v11 = "v11"
let v12 = "v12"
let v_out = "v.out"

let () =
  let vert = [v_in; v_out; v7; v8; v9; v10; v11; v12] in
  List.iter (fun v -> H.add_vertex g_func v ()) vert;
  H.add_hedge g_func "6.7" (Access_closure (a_id, func, 0)) ~pred:[|v_in|] ~succ:[|v7|];
  H.add_hedge g_func "7.8" (Access_param b_id) ~pred:[|v7|] ~succ:[|v8|];
  H.add_hedge g_func "8.9" (Add_int (a_id, b_id, c_id)) ~pred:[|v8|] ~succ:[|v9|];

  H.add_hedge g_func "9.10" (Set_closure (f_id, func, [|b_id|])) ~pred:[|v9|] ~succ:[|v10|];
  H.add_hedge g_func "10.11" (Prepare_call (f_id, c_id)) ~pred:[|v10|] ~succ:[|v11|];

  H.add_hedge g_func "11.12" Call ~pred:[|v11|] ~succ:[|v12|];

  H.add_hedge g_func "12.13" (Add_int (c_id, b_id, c_id)) ~pred:[|v12|] ~succ:[|v_out|];
  H.add_hedge g_func "9.12" (Set_int (c_id,m_cst)) ~pred:[|v9|] ~succ:[|v_out|]

let func_subgraph =
  { H.sg_input = [|v_in|];
    H.sg_output = [|v_out|];
    H.sg_vertex = vset_list [v7;v8;v9;v10;v11;v12];
    H.sg_hedge = hset_list ["6.7";"7.8";"8.9";"9.10";"10.11";"11.12";"12.13";"9.12"] }

let () = register_bigraph "bgints" (g,v0,g_func,func_subgraph)
