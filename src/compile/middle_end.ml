open Ast

open Display_internal_steps

let compile ?(prepare_lambda_lifting=true) ?(main=true) e =  

  (** renaming all names in the source program *)
  let e = Ast_rename.rename_e e in
  
  (** put the program in ANF-form *)
  let e = Anf.anf @@ e in
  display Anf ([],e);

  assert (Anf.in_anf e);


  (** move-up let-bindings (optional) *)
  let e = Let_floating.let_floating e in
  display Let_floating ([],e);

  assert (Anf.in_anf e);

  (** globalize functions *)
  let ds,e = Lambda_lifting.lambda_lifting ~prepare_lambda_lifting ~main e in
  display Lambda_lifting (ds,e);


  (* Typing.typing (List.fold_right (fun (x,e) acc -> 
               E_letIn(P_var x,e,acc)
          ) ds e);*)

  (** inlining of higher-order functions *)
  let (ds,e) = Specialize.specialize_main_n ds e in
  display Specialize (ds,e);


  let ds,e = Move_down_gfun_under_register.move_down_gfun_under_register (ds,e) in


  (** inlining of instantaneous functions *)
  let (ds,e) = Inline.inl (ds,e) in
  display Inline (ds,e);

  (** move-up let-bindings (optional) *)
  let e = Let_floating.let_floating e in

  (** copy/constant folding (optional) *)
  let (ds,e) = Propagation.propagation (ds,e) in
  display Propagation (ds,e);

  (* let (ds,e) = Norm_entry_point.norm_entry_point (ds,e) in *)

  let (ds,e) = List.map (fun (x,e) -> x,Matching.matching e) ds,Matching.matching e in
  display Matching (ds,e);

  let (ds,e) = Propagation.propagation (ds,e) in

  (** filtering [ds] to keep-only functions needed by [e] (and their dependancies). 
      Current compilation scheme to FSM (see [fsm_comp.ml]) produces ill-typed code 
      if this transformation is not performed *)
  let (ds,e) = Deadcode_elimination.deadcode_elimination (ds,e) in

  (* put a fresh name to each register *)
  let (ds,e) = Instantiate.instantiate_prog (ds,e) in
  
  (* Format.(fprintf std_formatter "==========>%a\n" Ast_pprint.pp_prog (ds,e));*)
(* assert false;*)
  (ds,e)
