(** A type-system, an interpreter and a hardware compiler 
   for a small language mixing general-purpose computation
   and instantaneous interaction 
 *)

(* input files *)
let inputs : string list ref = ref []

(* option configuration *)

let show_ty_and_exit_flag = ref false
let show_ast_and_exit_flag = ref false
let interp_flag = ref false
let top_flag = ref false
let relax_flag = ref false

let arguments = ref ""

(* main configuration *)
let () =
  let add_input (f:string) : unit = 
  inputs := !inputs @ [f]
  in
  Arg.parse [
    ("-main",    Arg.Set_string Ast_mk.main_symbol,
                 "entry point (a function name)");
   
    ("-top",     Arg.Set top_flag,
                 "Interaction loop");
   
    ("-int",     Arg.Int Fix_int_lit_size.set_size,
                 "force litteral integers to be of the given size");
  
    ("-interp",   Arg.Set interp_flag,
                 "interprete and exit.");
  
    ("-arg",      Arg.Set_string arguments,
                  "specify a list of inputs (one at each clock tick)\
                  \ for interpretation of the source program or simulation\
                  \ of the generated VHDL code");
  
    ("-ast",      Arg.Set show_ast_and_exit_flag,
                 "print input program and exit.");
  
    ("-ty",       Arg.Set show_ty_and_exit_flag,
                  "type and exit.");
  
    ("-pp",      Arg.String Display_internal_steps.set_print_mode, 
                 "display the output of the specified (intermediates)\
                 \ compilation pass specified.\n\tPossible values:\
                 \ [anf;float;lift;spec;inl;prop;match;middle-end;fsm;flat;encode].");
  
    ("-pp-fsm", Arg.String Display_target.set_print_mode,
                 "display the output of the specified (low-level)\
                 \ compilation pass.\n\tPossible values: [fsm;flat;encode].");
  
    ("-hexa",    Arg.Set Ast_pprint.hexa_int_pp_flag,
                 "printer using hexadecimal");
  
    ("-relax",   Arg.Set relax_flag,
                 "allow the main function to be non-instantaneous (such program is no longer reactive!)");

    ("-bus",     Arg.Set_int Interp.flag_bus_proba,
                "[for -interp mode only] set the probability to wait a new\
                 \ clock tick during a bus transation");
    ]
      add_input "Usage:\n  ./mixlang file"
;;

let main () : unit =
  (** Lexing/parsing of source code *)
  let (e,arg_list) = 
    Frontend.frontend ~inputs:!inputs !top_flag 
                      ~when_repl:Typing.when_repl
                      ~relax:!relax_flag
                      !Ast_mk.main_symbol
                      !arguments
  in

  (** Pretty print *)
  if !show_ast_and_exit_flag then begin
    Format.(fprintf std_formatter "@[<v>%a@,@]" Ast_pprint.pp_exp e);
    exit 0
  end;

  (** Typing *)
  let (ty,response_time) = Typing.typing_with_argument e arg_list in

  (** Type only, when [show_ty_and_exit_flag] is setted. *)
  if !show_ty_and_exit_flag then begin
    let open Ast_pprint in
    Format.fprintf Format.std_formatter "@,%a\n" pp_ty ty;
    exit 0;
  end;

  (** remove all decorations (locations) in the source program *)
  let (e, arg_list) =
    let open Ast_undecorated in
    (remove_deco e, List.map remove_deco arg_list)
  in

  (** Interprete only, when [interp_flag] is setted.  *)
  if !interp_flag then begin
      Interp.interp e arg_list |> ignore;
      exit 0
  end;

  (** standard compilation mode *)

  let name = "main" in
  let vhdl_name = "vhdl/"^name^".vhdl" in
  let oc_vhdl = open_out vhdl_name in
  let oc_tb = open_out ("vhdl/tb_"^name^".vhdl") in
  let fmt_vhdl = Format.formatter_of_out_channel oc_vhdl in
  let fmt_tb = Format.formatter_of_out_channel oc_tb in
  let (argument,result,typing_env) = Compile.compile name ty fmt_vhdl e in
  let args = (List.map Fsm_comp.to_a arg_list) in
  
  Gen_testbench.gen_testbench fmt_tb typing_env name ty (argument,result) args;
  
  Format.fprintf Format.std_formatter 
      "\nvhdl code generated in vhdl/main.vhdl\
      \ \ntestbench generated in vhdl/tb_main.vhdl for software RTL simulation using GHDL.\n";

  Gen_glue_code.gen_glue_code ();


  close_out oc_vhdl;
  close_out oc_tb ;



;;

(* enty point of the tool *)

let () =
  try main () with Prelude.Errors.Caml_error -> exit 1;;
