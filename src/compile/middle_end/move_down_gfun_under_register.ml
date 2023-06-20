
open Ast  

let map_under_register f e =
  let rec aux e = 
    match e with
    | E_deco _ ->
      Ast_undecorated.still_decorated e
    | E_var _ | E_const _ -> 
        e
    | E_if(e1,e2,e3) ->
        E_if(aux e1, aux e2, aux e3)
    | E_letIn(p,e1,e2) ->
        E_letIn(p,aux e1,aux e2)
    | E_app(e1,e2) ->
        E_app(aux e1,aux e2)
    | E_fun(p,e) ->
        E_fun(p,aux e)
    | E_fix(f,(p,e)) ->
        E_fix(f,(p,aux e))
    | E_tuple es ->
        E_tuple (List.map aux es)
    | E_lastIn(x,e1,e2) ->
        E_lastIn(x,aux e1,aux e2)
    | E_set(x,e1) ->
        E_set(x,aux e1)
    | E_step(e1,k) ->
        E_step(f e1,k)
    | E_par(e1,e2) ->
        E_par(f e1, f e2)
    | E_reg _ | E_exec _ ->
      assert false (* already expanded *)
  in aux e


let move_down_gfun_under_register_exp y ds e =
  let f e =
    let e_prepared_for_lambda_lifting = (Anf.anf e) in
    List.fold_right (fun (x,ex) e -> 
        if x = y then e else E_letIn(P_var x,ex,e)) ds e_prepared_for_lambda_lifting
  in
  map_under_register f e


let move_down_gfun_under_register (ds,e) =
  (List.map (fun (x, e) -> x, move_down_gfun_under_register_exp x ds e) ds, 
   move_down_gfun_under_register_exp (gensym()) ds e)