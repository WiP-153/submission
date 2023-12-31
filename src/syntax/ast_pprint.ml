open Ast


(* pretty printer *)

open Format ;;
set_ellipsis_text "<...>";;
set_max_boxes 20 ;;

let parenthesize ~paren pp fmt a =
 if paren then fprintf fmt "(%a)" pp a else pp fmt a

let rec size_is_not_zero = function
| T_size n -> n > 0
| T_add(t1,t2) | T_max(t1,t2) -> size_is_not_zero t1 || size_is_not_zero t2
| T_var _ -> false
| _ -> false

let pp_ty fmt ty =
  let h_assoc_tvars = Hashtbl.create 10 in
  let tvars_cur = ref 0 in
  let assoc_tvars n =
    match Hashtbl.find_opt h_assoc_tvars n with
    | Some v -> v 
    | None -> let v = !tvars_cur in incr tvars_cur; Hashtbl.add h_assoc_tvars n v; v in
  let rec pp_type ~paren fmt ty =
  let open Format in
  match ty with
  | T_const tc ->
      (match tc with
       | TInt tz -> fprintf fmt "int<%a>" (pp_type ~paren:false) tz
       | TBool -> fprintf fmt "%s" "bool"
       | TUnit -> fprintf fmt "%s" "unit")
  | T_var{contents=Unknown n} ->
     (match (assoc_tvars n) with
      | 0 -> fprintf fmt "'a"
      | 1 -> fprintf fmt "'b"
      | 2 -> fprintf fmt "'c"
      | 3 -> fprintf fmt "'d"
      | v -> fprintf fmt "'a%d" v)
  | T_var{contents=Ty t} ->
      fprintf fmt "%a" (pp_type ~paren) t
  | T_tuple ts ->
      (* Parentheses are needed to avoid confusion between tuples and pair nesting *)
      fprintf fmt "(";
      pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt " * ")
            (pp_type ~paren:true) fmt ts;
      fprintf fmt ")"
  | T_fun{arg;dur;ret} ->
      parenthesize ~paren (fun fmt () ->
      match dur with
      | T_size 0 ->
          fprintf fmt "%a => %a" 
            (pp_type ~paren:true) arg
            (pp_type ~paren:true) ret
      | t -> if size_is_not_zero t then ( 
                fprintf fmt "%a -> %a" 
                   (pp_type ~paren:true) arg
                   (pp_type ~paren:true) ret
              ) else (
                fprintf fmt "%a -[%a]-> %a" 
                  (pp_type ~paren:true) arg 
                  (pp_type ~paren:false) dur 
                  (pp_type ~paren:true) ret)
        ) fmt ()
  | T_array t ->
      fprintf fmt "%a array"
        (pp_type ~paren:true) t
  | T_string tz ->
      fprintf fmt "string<%a>" 
        (pp_type ~paren:false) tz
  | T_buffer{elem=t;size=tz} ->
      fprintf fmt "%a buffer<%a>" 
        (pp_type ~paren:true) t 
        (pp_type ~paren:false) tz
  | T_size n ->
      fprintf fmt "%d" n
  | T_infinity ->
      fprintf fmt "+∞"
  | T_add (tz1,tz2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "%a + %a" 
          (pp_type ~paren:true) tz1 
          (pp_type ~paren:true) tz2
        ) fmt ()
  | T_max (tz1,tz2) ->
      fprintf fmt "max(%a,%a)" 
      (pp_type ~paren:false) tz1
      (pp_type ~paren:false) tz2
  | T_le (tz1,tz2) ->
      parenthesize ~paren (fun fmt () ->
        fprintf fmt "t where t = %a & t <= %a"
          (pp_type ~paren:true) tz1 
          (pp_type ~paren:true) tz2
      )fmt ()
  in 
  pp_type ~paren:false fmt ty

let pp_ident fmt x =
  fprintf fmt "%s" x

let pp_op fmt op =
    let f = fprintf fmt "%s" in
    match op with
    | Add -> f "+"
    | Sub -> f "-"
    | Mult -> f "*"
    | Le -> f "<="
    | Lt -> f "<"
    | Ge -> f ">="
    | Gt -> f ">"
    | Eq -> f "=="
    | Neq -> f "!="
    | Not -> f "not"
    | And -> f "&&"
    | Or -> f "or"
    | Mod -> f "mod"
    | Div -> f "/"
    | Abs -> f "abs"
    | Wait n -> fprintf fmt "wait<%d>" n
    | GetTuple {pos=0;arity=2} -> f "fst"
    | GetTuple {pos=1;arity=2} -> f "snd"
    | GetTuple {pos=i;arity=_} -> fprintf fmt "get_tuple<%d>" i
    | Buffer_lit n -> fprintf fmt "buffer_lit<%d>" n
    | Buffer_make tz -> fprintf fmt "buffer_make<%a>" pp_ty tz
    | Buffer_get -> f "buffer_get"
    | Buffer_update -> f "buffer_update"
    | Buffer_length -> f "buffer_length"
    | String_get -> f "string_get"
    | String_length -> f "string_length"
    
    | Assert -> f "assert"
    | Random -> f "random"
    | Print -> f "print"
    | To_string -> f "to_string"
    | TyConstr ty -> fprintf fmt "(as_type %a)"  pp_ty ty

let pp_external fmt op =
  fprintf fmt @@
    match op with
    | Array_make -> "array_make"
    | Array_set -> "array_set"
    | Array_get -> "array_get"
    | Array_length -> "array_length"


let hexa_int_pp_flag = ref false


let rec pp_const fmt = function
| Int (n,_) -> 
    if !hexa_int_pp_flag then fprintf fmt "0x%x" n else fprintf fmt "%d" n
| Bool b -> fprintf fmt "%b" b
| Unit -> fprintf fmt "()"
| String s -> fprintf fmt "%s" s
| Op op ->
   fprintf fmt "(%a)"
      pp_op op
| External ext ->
   fprintf fmt "(%a)"
      pp_external ext
| V_loc l -> (* only for evaluation *) 
    fprintf fmt "#%a" pp_ident l

let rec pp_pat fmt p =
  match p with
  | P_unit ->
      fprintf fmt "()"
  | P_var x ->
      pp_ident fmt x
  | P_tuple ps ->
      fprintf fmt "(";
      pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt ", ")
            pp_pat fmt ps;
      fprintf fmt ")"

let rec pp_exp fmt e =
  match e with
  | E_deco(e,loc) ->  
      pp_exp fmt e
  | E_const c ->
      pp_const fmt c
  | E_var x ->
      pp_ident fmt x
  | E_fun (p,e) ->
       fprintf fmt "@[<v 2>(fun %a ->@,%a)@]" pp_pat p pp_exp e
  | E_fix (f,(p,e)) ->
       fprintf fmt "@[<v 2>(fix %a (fun %a ->@,%a))@]" pp_ident f pp_pat p pp_exp e
  | E_if(e,e1,e2) ->
      fprintf fmt "(@[<v>if %a@,then %a@,else %a@])"
        pp_exp e
        pp_exp e1
        pp_exp e2
  | E_letIn(p,e1,e2) ->
      fprintf fmt "(@[<v>let %a = %a@,in %a@])"
        pp_pat p
        pp_exp e1
        pp_exp e2
  | E_app(e1,e2) ->
      fprintf fmt "@[<v>(%a %a)@]" pp_exp e1 pp_exp e2
  | E_tuple es ->
     fprintf fmt "@[(";
      pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt ", ")
            pp_exp fmt es;
      fprintf fmt ")@]"
  | E_reg(V ev, e0) ->
      fprintf fmt "(@[<v>reg %a last %a@])"
        pp_exp ev pp_exp e0
  | E_exec(e1,e2,x) ->
      fprintf fmt "(@[<v>exec[%s] %a default %a@])"
        x pp_exp e1 pp_exp e2
  | E_lastIn(x,e1,e2) ->
      fprintf fmt "(@[<v>last %a = %a in %a@])" pp_ident x pp_exp e1 pp_exp e2
  | E_set(x,e) ->
      fprintf fmt "(@[<v>(%a <- %a)@])"
        pp_ident x pp_exp e
  | E_step(e,x) ->
      fprintf fmt "[step %a]^%s"
        pp_exp e x
  | E_par(e1,e2) ->
      fprintf fmt "(%a || %a)"
        pp_exp e1 pp_exp e2

let pp_prog fmt (ds,e) =
  fprintf fmt "@[<v>";
  List.iter (fun (x,e) -> fprintf fmt "let %s = %a in@," x pp_exp e) ds;
  fprintf fmt "%a@]" pp_exp e