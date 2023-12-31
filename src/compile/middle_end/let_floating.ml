open Ast
open Ast_subst


(**

goal :
given a program in ANF-form, e.g.
  [let (f,g) = let f1 = fun x -> e in
               let f2 = fun y -> e' in
               (f1,f2)
   in
   (f(g(3)))], systematically moves up bindings to obtain :

     [let f1 = fun x -> e in
      let f2 = fun y -> e' in
      let (f,g) = (f1,f2) in
      (f(g(3)))]

    which is needed (after copy propagation) for elimintation of high-order
    (ensuring in particular, above, that tuples do not contain functions. *)

(* must avoid scope extrusion and preserve the order of computations *)



let rec glob e =
  let open Ast in
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_const _ | E_var _ ->
      [],e
    | E_app(e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,E_app(e1',e2')
  | E_tuple(es) ->
      let dss,es' = List.map glob es |> List.split in
      List.concat dss,E_tuple(es')
  | E_fix(f,(x,e)) ->
      [],E_fix(f,(x,let_floating e))
  | E_fun(x,e) ->
      [],E_fun(x,let_floating e)
  | E_if(e1,e2,e3) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      let ds3,e3' = glob e3 in
      ds1@ds2@ds3,E_if(e1',e2',e3') (* ds2 and ds3 disjoints *)
  | E_letIn(p,e1,e2) ->
      let ds1,e1' = glob e1 in
      ds1@[(p,e1')],let_floating e2
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)
  | E_lastIn(x,e1,e2) ->
      [],E_lastIn(x,let_floating e1,let_floating e2)
  | E_set(x,e1) ->
      let ds1,e1' = glob e1 in
      ds1,E_set(x,e1')
  | E_step(e1,k) ->
      [],E_step(let_floating e1,k)
  | E_par(e1,e2) ->
      [],E_par(let_floating e1,let_floating e2)

and let_floating e =
  let bs, e = glob e in
  List.fold_right (fun (p,e) acc -> E_letIn(p,e,acc)) bs e
