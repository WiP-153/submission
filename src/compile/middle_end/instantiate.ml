open Ast
open Ast_subst
open Ast_rename


let rec instantiate e = 
  match e with
  | E_deco(e,ty) ->
      E_deco(instantiate e,ty)
  | E_const _ | E_var _ -> e
  | E_fun(p,e) ->
      E_fun(p,instantiate e)
  | E_fix(f,(p,e)) ->
      E_fix(f,(p,instantiate e))
  | E_tuple es ->
      E_tuple (List.map instantiate es)
  | E_app(e1,e2) ->
      E_app(instantiate e1,instantiate e2)
  | E_if(e1,e2,e3) ->
      E_if(instantiate e1,instantiate e2,instantiate e3)
  | E_letIn(p,e1,e2) ->
     E_letIn(p,instantiate e1,instantiate e2)
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,instantiate e1,instantiate e2)
  | E_set(x,e1) ->
      E_set(x,instantiate e1)
  | E_step(e1,_) -> 
      E_step(instantiate e1,gensym ())
  | E_par(e1,e2) ->
      E_par(instantiate e1,instantiate e2)
  | E_reg(V ev,e0) ->
      let y = gensym ~prefix:"instance" () in
      E_reg(V (E_fun(P_var y,E_app(ev,(E_var y)))),instantiate e0)
  | E_exec(e1,e2,_) ->
      E_exec(instantiate e1,instantiate e2,gensym ())


let instantiate_prog (ds,e) =
  List.map (fun (x,e) -> x,instantiate e) ds, instantiate e
