(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

open Data

let import_cp x =
  let open Int_interv in
  try
    { x with
      int =
        join_list
          [
            cst (Ints.min_elt x.cp);
            cst (Ints.max_elt x.cp);
            x.int ];
    }
  with Not_found -> x

(* ints *)

let restrict_intcp x =
  { bottom with
    int = x.int; cp = x.cp;
    expr = x.expr;
  }

let restrict_not_intcp x =
  { x with int = bottom.int; cp = bottom.cp; }

let singleton const =
  { bottom with
    int = Int_interv.cst const;
  }

let zero = singleton 0

let any =
  { bottom with int = Int_interv.top;  }

let join x y =
  { bottom with
    cp = Ints.union x.cp y.cp;
    int = Int_interv.join x.int y.int;
    expr = Hinfos.union x.expr y.expr;
  }

let meet x y =
{ bottom with
  cp = Ints.inter x.cp y.cp;
  int = Int_interv.meet x.int y.int;
  expr = Hinfos.inter x.expr y.expr;
  }

let add x y =
  let x = import_cp x
  and y = import_cp y in
  { bottom with int = Int_interv.add x.int y.int }

let op1 ( f : Int_interv.t -> Int_interv.t) x =
  let x = import_cp x in
  { bottom with int = f x.int }

let op2 ( f : Int_interv.t -> Int_interv.t -> Int_interv.t) x y =
  let x = import_cp x
  and y = import_cp y in
  { bottom with int = f x.int y.int }

let comp c x y =
  let x = import_cp x
  and y = import_cp y in
  begin
    match Int_interv.comp c x.int y.int with
    | Some true -> { bottom with cp = Ints.singleton 1 }
    | Some false -> { bottom with cp = Ints.singleton 0 }
    | None -> { bottom with cp = Ints.add 1 ( Ints.singleton 0 ) }
  end,
  restrict_intcp x,
  restrict_intcp y

let make_comp c x y =
  let xi, yi = Int_interv.make_comp c x.int y.int in
  { x with int = xi }, { y with int = yi }

let is_int env d =
  let res = Ints.empty in
  let res =
    if Ints.is_empty d.cp && Int_interv.is_bottom d.int
    then res
    else Ints.add 1 res
  in
  let res =
    if is_bottom { d with int = bottom.int; cp = bottom.cp; }
    then res
    else Ints.add 0 res
  in
  { bottom with cp = res ; }

let is_out m i =
  let m = import_cp m in
  let i = import_cp i in
  let res1, i1, _ = comp Lambda.Clt i zero
  and res2, i2, m2 = comp Lambda.Cgt i m in
  join res1 res2,
  m2,
  join i1 i2

let make_is_out b m i =
  let m = import_cp m in
  let i = import_cp i in
  if b
  then
    let i1,_ = make_comp Lambda.Clt i zero in
    let i2, m2 = make_comp Lambda.Cgt i m in
    m2, join i1 i2
  else
    let i1,_ = make_comp Lambda.Cge i zero in
    let i2, m2 = make_comp Lambda.Cle i m in
    m2, meet i1 i2

let of_interv i = { bottom with int = i }
