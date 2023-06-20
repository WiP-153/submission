open Ast
open Ast_subst

let rename_main_arg (ds,e) =
  ds,(match e with
      | E_fun(P_var x,e1) ->
          subst_e x (E_var "argument") e1
      | _ -> assert false)
