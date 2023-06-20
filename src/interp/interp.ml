open Ast
open Ast_subst

let flag_bus_proba = ref 10 ;;

Random.self_init ();;

let heap : (l,c array) Hashtbl.t = Hashtbl.create 10

let list_update n v' l = 
  let rec aux i = function
  | [] -> []
  | v::r -> if i = 0 then v'::r else v::aux (i-1) r
  in aux n l

let check_bounds ~index:i ~size:n =
if i < 0 || i >= n then 
  Prelude.Errors.error ~error_kw:"Runtime error" 
    (fun fmt -> Format.fprintf fmt "index out of bounds")

let error_cannot_be_reduced e =
  (* should not happen since the language must be type safe *)
  Prelude.Errors.error (fun fmt () -> 
                       Format.fprintf fmt "expression %a cannot be reduced." 
                       Ast_pprint.pp_exp e)

let app_const e e2 mu =
  let c = match e with
          | E_const c -> c 
          | _ -> error_cannot_be_reduced @@ E_app(e,e2) in
  match c with
  | Op op -> begin
      match op,e2 with
      | Add,  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Int (n+m,T_max(tz,tz'))), mu
      | Sub,  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Int (n-m,T_max(tz,tz'))), mu
      | Mult, E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Int (n*m,T_max(tz,tz'))), mu
      | Div,  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Int (n/m,T_max(tz,tz'))), mu
      | Mod,  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Int (n mod m,T_max(tz,tz'))), mu
      | Lt,   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Bool (n<m)), mu
      | Le,   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Bool (n<=m)), mu
      | Gt,   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Bool (n>m)), mu
      | Ge,   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Bool (n>=m)), mu
      | Eq,   E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Bool (n==m)), mu
      | Neq,  E_tuple [E_const (Int (n,tz)); E_const (Int (m,tz'))] -> E_const (Bool (n!=m)), mu
      | And,  E_tuple [E_const (Bool a); E_const (Bool b)] -> E_const (Bool (a&&b)), mu
      | Or,  E_tuple  [E_const (Bool a); E_const (Bool b)] -> E_const (Bool (a||b)), mu
      | Not,  E_const (Bool b) -> E_const (Bool (not b)),mu
      | Abs,  E_const (Int (n,tz)) -> E_const (Int (abs n,tz)),mu
      | GetTuple{pos=i;arity=n}, E_tuple vs -> 
          check_bounds ~index:i ~size:n;
          List.nth vs i,mu
      | Print,e ->
         assert (evaluated e);
         Format.fprintf Format.std_formatter "==> %a\n" Ast_pprint.pp_exp e; flush stdout; E_const Unit,mu
      | Random,E_const (Int (n,tz)) -> E_const (Int (Random.int n,T_size 32)),mu
      | Assert,E_const (Bool b) -> assert b; E_const Unit,mu 
      | Wait 0,v -> 
          v, mu
      | Wait n,v -> 
          E_app(E_const(Op(Wait (n-1))),v), mu
      | Buffer_lit n,E_tuple(es) -> E_app(E_const(Op(Buffer_lit n)),E_tuple(es)), mu
      | Buffer_make tz,v ->
          (* buffer_make applied to values is a value *)
          (match Typing.canon tz with
          | T_size n ->
              E_app(E_const(Op(Buffer_lit n)),E_tuple(List.init n (fun _ -> v))), mu
          | _ -> assert false) (* cannot infer buffer size *)
      | Buffer_get,E_tuple[E_app(E_const(Op(Buffer_lit n)),E_tuple(es));
                           E_const (Int (i,_))] ->
          check_bounds ~index:i ~size:n;
          List.nth es i, mu
      | Buffer_update,E_tuple[E_app(E_const(Op(Buffer_lit n)),E_tuple(es));
                           E_const (Int (i,_));v] -> 
          check_bounds ~index:i ~size:n;
          E_app(E_const(Op(Buffer_lit n)),E_tuple(list_update i v es)), mu
      | Buffer_length,E_app(E_const(Op(Buffer_lit n)),E_tuple(es)) -> 
          assert (n = List.length es);
          E_const(Int (List.length es,T_size 32)), mu
      | String_length,E_const(String s) -> E_const(Int (String.length s,T_size 32)), mu
      | TyConstr _, v -> v,mu
      | _ -> error_cannot_be_reduced (E_app(e,e2))
    end
  | (Unit|Bool _|Int _|String _|V_loc _) ->
      error_cannot_be_reduced (E_app(e,e2))
  | External ext -> 
      let n = Random.int !flag_bus_proba in
      let v = match ext,e2 with
              | Array_make,E_tuple [E_const (Int (n,_)); E_const c] ->
                  let arr = Array.make n c in
                  let l = gensym ~prefix:"l" () in
                  Hashtbl.add heap l arr;
                  E_const (V_loc l)
              | Array_length,E_const (V_loc l) -> 
                  let n = match Hashtbl.find_opt heap l with
                          | None -> assert false
                          | Some arr -> Array.length arr
                  in E_const (Int (n,T_size 32))
              | Array_get, E_tuple[E_const (V_loc l);E_const (Int (i,_))] ->
                  let c = match Hashtbl.find_opt heap l with
                          | None -> assert false
                          | Some arr -> Array.get arr i
                  in E_const c
              | Array_set, E_tuple[E_const (V_loc l);E_const (Int (i,_));E_const c0] ->
                  (match Hashtbl.find_opt heap l with
                   | None -> assert false
                   | Some arr -> Array.set arr i c0);
                  E_const Unit
              | (Array_make|Array_length|Array_get|Array_set),_ -> error_cannot_be_reduced (E_app(e,e2)) in
      E_app(E_const(Op(Wait n)),v),mu


open Format
let fmt = Format.std_formatter

let rec red (e,r) =
(*fprintf fmt "%a\n" pp_exp  e ; 
   (fprintf fmt "i: ";
             SMap.iter (fun x e -> fprintf fmt "    (%s %a)\n" x pp_exp e) r;
             fprintf fmt "@,");*)
  match e with 
  | E_deco(e1,_) -> red (e1,r) 
  | E_const _ | E_fun (_, _) | E_fix (_, _) ->
      assert (evaluated e); 
      (e,r)
  | E_var x -> (match SMap.find x r with
                | v -> v,r
                | exception Not_found -> 
                    Prelude.Errors.raise_error ~msg:("unbound variable "^x) ())
  | E_if(e,e1,e2) -> 
     (match red (e,r) with
     | E_const (Bool true),r' -> 
        (* [If-true] *)
        red (e1,r')
     | E_const (Bool false),r' -> 
        (* [If-false] *)
        red (e2,r')
     | (e',r') ->
        (* [If-pause] *)
        E_if(e',e1,e2),r')
  | E_letIn(p,e1,e2) ->
    let e1',r' = red (e1,r) in
    if evaluated e1' 
    then (* [Let-val] *)
      red (subst_p_e p e1' e2,  r')
    else (* [Let-pause] *)
      (E_letIn(p,e1',e2),r')
  | E_tuple es ->
      let es',r' = List.fold_right (fun e (acc,r) -> let e',r' = red (e,r) in (e'::acc),r') es ([],r) in
      (* fprintf fmt "%a ~> %a\n" Ast_pprint.pp_exp  (E_tuple es) Ast_pprint.pp_exp (E_tuple es') ; *)
      E_tuple es',r'
  | E_app(e1,e2) ->       
      let e2',r' = red (e2,r) in
      if not (evaluated e2')
      then (* [App-pause] *)
        (E_app(e1,e2'), r')
      else 
        let v = e2' in
        let e1',r'' = red (e1,r') in
        (match e1' with
         | E_const c -> app_const e1' v r''
         | E_fun(p,e) ->
             red (subst_p_e p v e,r'')
         | (E_fix(g,(p,e))) as w ->
            (subst_e g w @@
             subst_p_e p v e),r''
         | _ -> assert false)
  | E_reg(V ev,e0) ->
      (* [Reg] *)
      let v0,r = red (e0,r) in
      let y = match Ast.un_deco ev with 
              | E_fun(P_var x,_) -> x 
              | _ -> assert false (* should be normalized as a function *) in
      let v = match SMap.find_opt y r with
              | None -> v0 
              | Some v -> v in
      let v',r = red (E_app(ev,v),r) in
      v', SMap.add y v' r
  | E_set(x,e1) ->
     (* [Set] *)
     let v,r' = red (e1,r) in
     (E_const Unit, SMap.add x v r')
  | E_lastIn(x,e1,e2) ->
     (* [LastIn] *)
     let v,r' = red (e1,r) in
     let r'' = SMap.add x v r' in
     red (e2,r'')
  | E_step (e1,_) ->
     let x = gensym () in (* todo, pu x in E_step(e1,x) *)
     (* [LastIn] *)
     let e1' = match SMap.find_opt x r with
              | None -> e1
              | Some e2 -> e2 in
      let e1'',r' = red (e1',r) in
      let e1'' = if evaluated e1'' then e1 else e1'' in
      (E_const Unit, SMap.add x e1'' r)
  | E_exec (e1,e2,k) ->
     (* [Exec] *)
     if not (SMap.mem k r) then
        (* [Exec-init] *)
        let r' = SMap.add k e1 r in
        red (E_exec (e1,e2,k),r')
      else
        let e = SMap.find k r in
        let e',r' = red (e,r) in
        if evaluated e' then
          (* [Exec-val] *)
          let e1',r' = red (e1,r') in
          E_tuple[e';E_const(Bool true)], (SMap.add k e1' r')
        else
          (* [Exec-exp] *)
          let v2,r'' = red (e2,r') in
          assert (evaluated v2);
          E_tuple[v2;E_const(Bool false)], (SMap.add k e' r'')
  | E_par(e1,e2) -> 
      if evaluated e1 && evaluated e2 then E_tuple[e1;e2],r else
      let e1',r1 = red (e1,r) in
      let e2',r2 = red (e2,r1) in
      E_par(e1',e2'),r2
let rec reduce_until (e_init,mu) args =
  let rec aux (e,mu) args =
    match args with
    | [] -> (e,mu)
    | a::args' ->
        let ee = if evaluated e then E_app(e_init,a) else e in
        (* SMap.iter (fun x e -> fprintf fmt "~~~~~> (%s %a)\n" x Ast_pprint.pp_exp e) mu; *)
        let e',mu' = red (ee,mu) in
        let open Prelude.Errors in
        let open Format in
        (if evaluated e' then
          fprintf std_formatter "%a --> %a @]\n" Ast_pprint.pp_exp a Ast_pprint.pp_exp e'
        else    
          fprintf std_formatter "%a %a\n" Ast_pprint.pp_exp a (emph red) "(running)");
        aux (e',mu') args'
  in aux (e_init,mu) args



let interp (e : e) (value_list : e list) : (e * e env) =
  let e = Norm.normalize e in
  (* Format.(fprintf std_formatter "----> %a\n" Ast_pprint.pp_exp e);*)
  let mu = SMap.empty in
   (*  Format.(fprintf std_formatter "----> %a\n" Ast_pprint.pp_exp e); *)
  reduce_until (e,mu) value_list


(* *************** evaluate a close expression *************** *)

let eval e =
  let mu0 = SMap.empty in
  let rec aux (e,mu) =
    if evaluated e then e else aux (red (e,mu))
  in aux (e,mu0)



