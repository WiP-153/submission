open Fsm_syntax

let emit_warning_flag = ref false

(** [canon t] put the type [t] in canonical form by replacing 
  instantiated variables free in [t] by their definition, 
  themselves put in canonical form. *)
let rec canon = function
  | TVar{contents=V _} as t -> t
  | TVar{contents=T t} -> canon t
  | TInt tz -> TInt (canon tz)
  | TBool | TUnit | TString _ as t -> t
  | TTuple ts -> TTuple(List.map canon ts)
  | TSize _ as t -> t

(** [size_ty t] returns the size (in number of bytes) of type [t]
  Unspecified size are fixe to 32 bits by default 
  (customizable via argument [?when_tvar]) *)
let rec size_ty = 
  let seen = ref false in
  let when_tvar = 32 in
  fun t ->
    match canon t with
    | TInt n -> size_ty n
    | TBool -> 1
    | TUnit -> 1
    | TTuple ts -> List.fold_left (+) 0 (List.map size_ty ts)
    | TVar _ -> 
        if not !seen then begin seen := true;
          let open Prelude.Errors in
          if !emit_warning_flag then warning (fun fmt -> Format.fprintf fmt "Unknown value size in the generated code replaced by a %d bits range size.\n" when_tvar);
        end;
        when_tvar
    | TString tz -> (size_ty tz * 8)
    | TSize n -> n


let rec string_of_ty = function
  | TInt tz -> "int<"^string_of_ty tz^">"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TString tz -> "string<"^string_of_ty tz^">"
  | TTuple ts -> "("^(String.concat "*" @@ List.map string_of_ty ts)^")"
  | TVar{contents=T t} -> string_of_ty t
  | TVar{contents=V s} -> s
  | TSize n -> "size<"^string_of_int n^">"

exception CannotUnify of (ty*ty)

let rec unify t1 t2 =
  (* todo: check cycle (eg. 'a ~ ('a * 'a)) *)
  (* Printf.printf "%s" ("---->"^("unify "^string_of_ty(canon t1)^" and "^string_of_ty (canon t2) ^"\n")); *)
  match canon t1,canon t2 with
  | TInt tz1, TInt tz2 -> 
      if not (Fix_int_lit_size.is_set ()) then () else 
      begin
         unify tz1 (TSize (Fix_int_lit_size.get_size_type ()))
      end;
      unify tz1 tz2
  | TSize n, TSize m -> if n <> m then raise (CannotUnify(t1,t2))
  | TBool,TBool -> ()
  | TUnit,TUnit -> ()
  | TTuple ts,TTuple ts' ->
    if List.compare_lengths ts ts' <> 0 then raise (CannotUnify(t1,t2))
    else List.iter2 unify ts ts'
  | TVar ({contents=V n} as r),TVar {contents=V n'} -> if n = n' then () else r := T t2; ()
  | TVar {contents=T t1},t2 | t1,TVar {contents=T t2} -> unify t1 t2
  | TVar ({contents=V _} as r),t | t,TVar ({contents=V _} as r) ->
    r := T t
  | TString tz,TString tz' -> 
      unify tz tz'
  | t1,t2 -> raise (CannotUnify(t1,t2)) 

let add_typing_env h (x:string) (t:ty) =
  (match Hashtbl.find_opt h x with
   | None -> Hashtbl.add h x t
   | Some t' -> unify t t')

let typing_c h = function
  |  Unit -> TUnit
  |  (Int{value=_;tsize=tz}) -> 
       if not (Fix_int_lit_size.is_set ()) then () else begin
         unify tz (TSize (Fix_int_lit_size.get_size_type ()))
       end;
       TInt tz
  |  (Bool _) -> TBool
  |  (Enum _) -> (new_tvar ()) (* TODO! *)
  |  (String s) -> TString (TSize(String.length s))

let rec typing_op h t op =
    match op with
     | (Add|Sub|Mult|Div|Mod) ->
       let tz = new_tvar () in
       unify (TTuple [TInt tz;TInt tz]) t;
       TInt tz
     | (Eq|Neq|Lt|Le|Gt|Ge) ->
       let tz = new_tvar () in
       unify (TTuple [TInt tz;TInt tz]) t;
       TBool
     | (And|Or) ->
         unify (TTuple [TBool;TBool]) t; 
         TBool
     | Not -> unify t TBool; TBool
     | If -> 
         let a = new_tvar () in 
         unify (TTuple [TBool;a;a]) t; 
         a
     | GetTuple(i,n,ty) -> 
        unify ty t; 
        let ts = List.init n (fun _ -> new_tvar()) in
        unify ty (TTuple (ts));
        List.nth ts i
     | To_string -> assert false
     | TyConstr ty ->
         unify ty t;
         t
     | String_length tz ->
         unify (TString tz) t;
         TInt(new_tvar())
     | Compute_address ->
         let w = (new_tvar ()) in
         unify (TTuple [TInt (TSize 32);TInt w]) t;
         t

let trace_last_exp = ref (A_const Unit)

let rec typing_a h a =
  trace_last_exp := a;
  match a with
  | A_const c ->
      typing_c h c
  | A_var x ->
      let t = (new_tvar ()) in
      add_typing_env h x t;
      t 
  | A_call(To_string,a) ->
       let t = typing_a h a in
       TString t
  | A_call(op,args) ->
      let t = typing_a h args in
      typing_op h t op
  | A_tuple es ->
      TTuple (List.map (typing_a h) es)
  | A_letIn(x,a1,a2) ->
      let t = typing_a h a1 in
      add_typing_env h x t; 
      typing_a h a2
  | A_string_get(sx,ix) ->
      add_typing_env h sx (TString (new_tvar())); 
      add_typing_env h ix (TInt (TSize 32)); 
      TInt(TSize 8)



let rec typing_s ~result h = function
  | S_set(_,x,a) ->
      let t = typing_a h a in
      (* (Format.fprintf Format.std_formatter "======> (%s : %a)\n" x Fsm_syntax.Debug.pp_ty (canon t)); *)
      add_typing_env h x t
  | S_if(a,s,so) ->
      unify TBool (typing_a h a);
      typing_s ~result h s; 
      Option.iter (typing_s ~result h) so
  | S_case(a,hs) ->
      let t = typing_a h a in
      List.iter (fun (c,s) -> unify (typing_c h c) t; 
                            typing_s  ~result h s) hs
  | S_seq(s1,s2) ->
      typing_s  ~result h s1; typing_s  ~result h s2
  | S_continue(f,a,_) -> 
      let t = typing_a h a in
      let f_arg = Naming_convention.formal_param_of_fun f in
      add_typing_env h f_arg t
  | S_return a ->
      let t = typing_a h a in 
      (* Printf.printf "----->%s\n\n\n" (string_of_ty @@ canon t);*)
      add_typing_env h result t
  | S_letIn(x,a,s) ->
      (add_typing_env h x (typing_a h a));
      typing_s  ~result h s
  | S_fsm(_,result2,ts,s,_) ->
      typing h ~result:result2 (ts,s)
  | S_print(a) -> 
      let _ = typing_a h a in
      ()
(* typing of an fsm *)
and typing h ~result (ts,s) =
  typing_s ~result h s;
  List.iter (fun (q,s) -> 
      (* add_typing_env h q (new_tvar());  *)
      typing_s ~result h s) ts;
  let xs = List.map fst ts in
  List.iter (fun x -> Hashtbl.remove h x) xs

let rec translate_ty t =  
  match t with
| Ast.T_const(TInt tz) -> TInt (translate_ty tz)
| Ast.T_const(TBool) -> TBool
| Ast.T_const(TUnit) -> TUnit
| Ast.T_tuple(ts) -> TTuple (List.map translate_ty ts)
| Ast.T_var _ -> new_tvar () (* todo: equivalence occurences *)
| Ast.T_string tz ->
    (match Typing.canon tz with
    | T_size n -> TString (TSize (n*8))
    | _ -> assert false) (* TODO *)
| Ast.T_size n -> TSize n
| Ast.T_array n -> TInt (TSize 32)
| _ -> assert false

let typing_circuit ty (result,fsm) =
  try
  let h = Hashtbl.create 64 in
  
  typing h ~result fsm;

  let t1,t2 = match ty with Ast.T_fun{arg=t1;dur=_;ret=t2} -> t1,t2 | _ -> assert false (* err *)
  in


  add_typing_env h "argument" (translate_ty @@ Typing.canon t1);   (* NB: does not work without canon *)
  add_typing_env h result (translate_ty @@ Typing.canon t2); 
  h
  with CannotUnify(t1,t2) -> 
      let open Prelude.Errors in
      error (fun fmt ->
      Format.fprintf fmt "@,In the generated code, expression %a has type %a but an expression was expected of type %a"
                 (emph_pp purple Debug.pp_a) !trace_last_exp
                 (emph_pp green Debug.pp_ty) t1 
                 (emph_pp green Debug.pp_ty) t2)
 