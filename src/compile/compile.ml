open Fsm_syntax
open Fsm_comp

module D = Display_internal_steps

let print_elaborated_code_flag = ref true

let compile name ty fmt e =

  D.display D.Front ([],e);

  let e = Encode_reg_exec.encode e in

  D.display D.Encode ([],e);

  let ds,e = Middle_end.compile e in
  let (ds,e) = Rename_main_arg.rename_main_arg (ds,e) in

  D.display D.MiddleEnd (ds,e);

  let (result,infos,fsm) as design = Fsm_comp.compile ~result:"result" (ds,e) in
  Display_target.(display Fsm fsm);

  let fsm = Flat_let_atom.flat_let_atom fsm in
  Display_target.(display Flat fsm);
  
  let typing_env = Fsm_typing.typing_circuit ty (result,fsm) in

  (* Hashtbl.iter (fun x t -> Printf.printf "===> (%s,%s)\n" x (Fsm_typing.string_of_ty (Fsm_typing.canon t))) typing_env; *)

  let name = "main" in
  let state_var = "state" in
  let argument = "argument" in
  let compute = "Compute" in
  let rdy = "rdy" in

  let fsm = Encode.encode_all ~result:(Delayed,result) ~compute ~state_var ~rdy:(Some(Delayed,rdy)) fsm in
  Display_target.(display Encode fsm);

  let (argument,result) = Gen_vhdl.pp_component fmt ~name ~state_var ~argument ~result ~compute ~rdy typing_env infos fsm in
  (argument,result,typing_env)
