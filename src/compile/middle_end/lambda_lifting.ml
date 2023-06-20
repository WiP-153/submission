
open Ast
open Free_vars

(** globalize all functions, by Lambda-lifting [Johnsson, 1982]:

    Given expression [e] in ANF form, return a program [(ds,e)] in ANF-form
    where all functions are defined in [ds].

    Our version of the algorithmic works in three steps:
    - [prepare]: add a second parameter at each function definition and function call
      to represent the lexical environment of the function (initialy empty)
    - [lift]: add free variables of each function definition, as a tuple,
      into the second parameter (environment) of the function definition
      and all these call. [lift] has to be repeat until all functions are close
    - [glob]: globalize all (close) functions.
      The order of globalized bindings is important!
        Indeed, global functions are not mutually-recursive:
        -- semantics is different for call of recursive or non recursive functions,
        -- there is only the construct [fix] of the language to encode recursion.
**)

(** [has_changed] boolean flag setted to true each time a [lift] pass modifies
    the input expression *)
let has_changed = ref false

let rec lift env e =
  let open Ast in
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ ->
      e
  | E_const _ ->
      e
  | E_app(E_const _,xc2) ->
      assert (Anf.is_xc xc2);
      e
  | E_app(E_var f,e1) ->
      (match SMap.find_opt f env with
       | None | Some (P_unit) -> E_app(E_var f,e1)
       | Some p ->
          E_app(E_var f,E_tuple[e1; pat2exp p]))
  | E_app _ -> assert false
  | E_if(exc1,e2,e3) ->
      assert (Anf.is_xc exc1);
      E_if(exc1, lift env e2, lift env e3)
  | E_letIn(P_var f,(E_fun(p,e1) as phi),e2) ->
      let e1' = lift env e1 in
      let xs = fv phi in
      let vp = (vars_of_p p) in
      let p_env' = xs |> SMap.filter (fun x _ -> not (SMap.mem x vp) && not (SMap.mem x env)) 
                      |> SMap.bindings 
                      |> List.map (fun (x,_) -> P_var x) 
                      |> group_ps in
      if not (SMap.is_empty (vars_of_p p_env')) then 
        (has_changed := true;
         let env2, ef = (SMap.add f p_env' env, E_fun(P_tuple[p;p_env'],e1')) in
         E_letIn(P_var f,ef,lift env2 e2) )
      else 
         let env2 = SMap.add f p_env' env in  
         E_letIn(P_var f,(E_fun(p,e1')),lift env2 e2)
  | E_letIn(P_var f,(E_fix(g,(p,e1)) as phi),e2) ->
      (if f <> g then let e1' = Ast_subst.subst_e g (E_var f) e1 in
                      let e2' = Ast_subst.subst_e g (E_var f) e2 in
                      lift env (E_letIn(P_var f,(E_fix(f,(p,e1'))),e2'))
      else
      let e1' = lift env e1 in
      let xs = fv phi in
      let vp = (vars_of_p p) in
      let p_env' = xs |> SMap.filter (fun x _ -> not (SMap.mem x vp) && not (SMap.mem x env)) 
                      |> SMap.bindings 
                      |> List.map (fun (x,_) -> P_var x) 
                      |> group_ps in
      if not (SMap.is_empty (vars_of_p p_env')) then ( has_changed := true;
        let env2,ef = (SMap.add f p_env' env, E_fix(f,(P_tuple[p;p_env'],e1'))) in
        E_letIn(P_var f,ef,lift env2 e2) )
       else 
        let env2 = SMap.add f p_env' env in  
        E_letIn(P_var f,(E_fix(f,(p,e1'))),lift env2 e2) )
  | E_letIn(p,e1,e2) ->
      E_letIn(p,lift env e1,lift env e2)
  | E_fun _ as fe ->
      let f = gensym ~prefix:"main" () in
      lift env (E_letIn(P_var f,fe,E_var f))
      (* E_fun(x,lift env e) *)
  | E_fix(f,_) as fe ->
      lift env (E_letIn(P_var f,fe,E_var f)) (* ok if fv(fe) not empty ? *)
  | E_tuple es_atoms ->
      E_tuple es_atoms
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,lift env e1,lift env e2)
  | E_set(x,exc1) ->
      assert (Anf.is_xc exc1);
      e
  | E_step(e1,k) ->
      E_step(lift env e1,k)
       | E_par _ -> e (* do not transform sub-expressions under step and // *)
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)
 

(** lifting has to be perform several time until reaching a fixpoint,
   then [has_changed] remains false *)
let rec lift_until e =
  has_changed := false;
  let e' = lift SMap.empty e in
  if !has_changed then lift_until e' else e'



let rec glob e =
  let open Ast in
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_const _ | E_var _ | E_tuple _ | E_app _ ->
      assert (Anf.in_anf e);
      [],e
  | E_letIn(P_var f,(E_fix _ | E_fun _ as v),e2) ->
      let dsv,v = glob v in
      let ds,e2' = glob e2 in
      (dsv@[(f,v)]@ds),e2'
  | E_fix(f,(p,e1)) ->
      let ds1,e1' = glob e1 in
      ds1,E_fix(f,(p,e1'))
  | E_fun(p,e1) ->
      let ds1,e1' = glob e1 in
      ds1,E_fun(p,e1')
  | E_if(xc1,e2,e3) ->
      assert (Anf.is_xc xc1); (* assume ANF *)
      let ds2,e2' = glob e2 in
      let ds3,e3' = glob e3 in
      ds2@ds3,E_if(xc1,e2',e3') (* ds2 and ds3 disjoint *)
  | E_letIn(p,e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,E_letIn(p,e1',e2')
  | E_lastIn(x,e1,e2) ->
      let ds1,e1' = glob e1 in
      let ds2,e2' = glob e2 in
      ds1@ds2,E_lastIn(x,e1',e2')
  | E_set(x,e1) ->
      assert(Anf.is_xc e1);
      [],e
  | E_step _ | E_par _ -> 
      (* do not transform sub-expressions under step and // *)
      [],e
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)

let lambda_lifting ~prepare_lambda_lifting ~main e =
    let e_lifted = (lift_until e) in
    let ds',e_globalized = glob e_lifted in
    (ds',e_globalized)
