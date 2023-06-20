
open Ast

(* ensure that the entry point is a function of the form (fun x -> e) *)

let norm_entry_point (ds,e) =
  ds,(match e with
      | E_fun _ -> e
      | E_var f ->
         let x = gensym () in
         E_fun(P_var x,E_app(E_var f,E_var x))
      | e ->
         let x = gensym () in
         (* function that does not use its argument x *)
         E_fun(P_var x,e))
