open Ast

let pp_ty = Ast_pprint.pp_ty

let unknown =
  let c = ref 0 in
  fun () ->
    let ty = T_var (ref (Unknown (!c))) in
    incr c; ty 

let simplify_size_constraints t =
  let rec simpl t = match t with
  | T_size _ | T_infinity -> t
  | T_add(t1,t2) ->
      (match simpl t1, simpl t2 with
      | T_add(ta,tb),t -> simpl @@ T_add(ta,T_add(tb,t))
      | T_size n,T_add(T_size m,tb) -> simpl @@ T_add(T_size (n+m),tb)
      | T_var _ as t1',t2' 
      | t2',(T_var _ as t1') -> T_add(t2',t1')
      | T_size n,T_size m -> T_size (n+m)
      | T_infinity,T_size _
      | T_infinity,T_infinity
      | T_size _ , T_infinity -> T_infinity
      | t,T_size 0 | T_size 0,t -> t
      | t1',t2' -> T_add(t1',t2'))
  | T_max(t1,t2) ->
      (match simpl t1, simpl t2 with
      | T_size n,T_size m -> T_size (max n m)
      | T_infinity,T_size _
      | T_infinity,T_infinity
      | T_size _ , T_infinity -> T_infinity
      | t,T_size 0 | T_size 0,t -> t
      | t1',t2' -> T_max(t1',t2'))
  | T_le _ -> failwith "todo T_le"
  | T_var({contents=Ty t'} as v) ->
      let t2 = simpl t' in
      v := Ty t2; t2
  | T_var{contents=Unknown _} ->
      t
  | _ -> assert false (* hill kinded *)
  in simpl t

(** [canon t] put the type [t] in canonical form by replacing 
  instantiated variables free in [t] by their definition, 
  themselves put in canonical form. *)
let rec canon t =
  match t with
  | T_const _ -> t
  | T_var({contents=Ty t'} as v) ->
      let t2 = canon t' in
      v := Ty t2; t2
  | T_var{contents=Unknown _} ->
      t
  | T_tuple (ts) ->
      T_tuple (List.map canon ts)
  | T_fun{arg;dur;ret} ->
      T_fun{ arg = canon arg; 
             dur = canon dur;
             ret = canon ret }
  | T_array t -> T_array (canon t)
  | T_string tz -> T_string (canon tz)
  | T_buffer {elem=t;size=tz} -> T_buffer {elem=canon t;size=canon tz}
  | (T_size _ | T_infinity | T_add _ | T_max _ | T_le _) as t -> simplify_size_constraints t 


let rec occur v ty =
  let exception Found in
  let rec f = function
  | T_var {contents=Unknown v'} ->
      if v = v' then raise Found 
  | T_var {contents=Ty ty} -> f ty
  | T_const _ -> ()
  | T_tuple ts ->
      List.iter f ts
  | T_fun{arg;dur;ret} ->
      f arg; f dur; f ret
  | T_array t -> f t
  | T_string tz ->
      f tz
  | T_buffer{elem=t;size=tz} -> 
      f t; f tz
  | (T_size _ | T_infinity) -> ()
  | T_max(tz1,tz2) | T_add(tz1,tz2) | T_le(tz1,tz2) -> 
      f tz1; f tz2
  in try f ty; false with Found -> true

  module Vs = Set.Make(Int)

  type scheme = Forall of (Vs.t * ty)

  let vars_of_type t =
    let rec vars s = function
    | T_const _ -> s
    | T_tuple ts ->
       List.fold_left vars s ts
    | T_var vt ->
         (match !vt with
             Unknown n ->
               Vs.add n s
           | Ty t ->
               vars s t)
    | T_fun{arg;dur;ret} ->
        vars (vars (vars s arg) dur) ret
    | T_array t ->
        vars s t
    | T_string tz ->
        vars s tz
    | T_buffer{elem=t;size=tz} ->
        vars (vars s t) tz
    | T_size _ | T_infinity -> s
    | T_max(t1,t2) | T_add(t1,t2) | T_le(t1,t2) -> 
        vars (vars s t1) t2
    in
    vars Vs.empty t

  let free_vars_of_type (bv,t) =
    Vs.diff (vars_of_type t) bv

  let instance (Forall(vs,ty)) =
    let unknowns = Hashtbl.create (Vs.cardinal vs) in
    Vs.iter (fun n -> Hashtbl.add unknowns n (unknown())) vs;
     let rec instance = function
       T_var {contents=(Unknown n)} as t ->
          (try Hashtbl.find unknowns n with Not_found -> t)
     | T_var {contents=(Ty t)} ->
         instance t
     | (T_const _) as t ->
         t
     | T_array t ->
         T_array(instance t)
     | T_tuple ts ->
         T_tuple(List.map instance ts)
     | T_fun{arg;dur;ret} ->
          T_fun{ arg = instance arg; 
                 dur = instance dur;
                 ret = instance ret }
     | T_string tz ->
        T_string(instance tz)
     | T_buffer{elem=t;size=tz} ->
        T_buffer{elem=instance t;size=instance tz}
     | (T_size _ | T_infinity) as t -> t
     | T_max(t1,t2) -> T_max(instance t1,instance t2)
     | T_add(t1,t2) -> T_add(instance t1,instance t2)
     | T_le(t1,t2) -> T_le(instance t1,instance t2)
     in
       instance ty

  type env = scheme SMap.t

  let free_vars_of_type_env l =
    List.fold_left (fun vs (x,(Forall (v,t))) ->
                    Vs.union vs (free_vars_of_type (v,t)) )
      Vs.empty l

  let generalize r ty =
    let fvg = free_vars_of_type_env r in
    Forall(free_vars_of_type (fvg,ty),ty)

  let tvar_a = T_var (ref (Unknown (-1)))
  let tvar_b = T_var (ref (Unknown (-2)))
  let tvar_z = T_var (ref (Unknown (-3)))

  let id = function
  | T_var {contents=Unknown n} -> n
  | _ -> assert false

  let forall vs t =
    Forall(Vs.(of_list vs),t)



exception Cyclic of int * ty * Prelude.loc
exception CannotUnify of ty * ty * Prelude.loc


(* unify t1 and t2 with subtyping: t1 <= t2 *)
let rec unify ~loc t1 t2 =
  let unify_tconst tc1 tc2 =
    match tc1,tc2 with
    | TInt tz, TInt tz' ->
        if not (Fix_int_lit_size.is_set ()) then () else begin
          unify ~loc tz (T_size (Fix_int_lit_size.get_size_type ()))
         end;
        unify ~loc tz tz'
    | TBool,TBool | TUnit,TUnit -> ()
    | _ -> raise @@ CannotUnify(t1,t2,loc)
  in
  (* Format.(fprintf std_formatter "%a / %a\n" pp_ty  t1 pp_ty t2); *)
  match canon t1, canon t2 with
  | T_var {contents=(Unknown n)},
    T_var ({contents=Unknown m} as v) ->
      if n = m then () else v := Ty t1
  | T_var ({contents=Ty t1'}),
    T_var ({contents=Ty t2'}) ->
       unify ~loc t1' t2'
  | T_var ({contents=Ty t} as v),t' ->
       v := Ty t';
       unify ~loc t t'
  | T_var ({contents=Unknown n} as v),t ->
      if occur n t then raise (Cyclic(n,t,loc));
      v := Ty t
  | t1,(T_var _ as t2) ->
       (** NB: [t2 and t1] are swapped to preserve the direction 
           of the subtyping relation (i.e. [t1 <= t2])
        *)
       unify ~loc t2 t1
  | T_const tc, T_const tc' ->
      unify_tconst tc tc'
  | T_tuple ts, T_tuple ts' ->
      List.iter2 (unify ~loc) ts ts'
  | T_fun{arg;dur;ret},T_fun{arg=a;dur=d;ret=r} ->
      unify ~loc arg a;
      unify ~loc dur d;
      unify ~loc ret r
  | T_array t,T_array t' ->
      unify ~loc t t'
  | T_string tz,T_string tz' ->
      unify ~loc tz tz'
  | T_buffer{elem=t;size=tz},T_buffer{elem=t';size=tz'} ->
      unify ~loc t t';
      unify ~loc tz tz'
  | (T_size _ | T_infinity | T_add _ | T_max _ | T_le _),
    (T_size _ | T_infinity | T_add _ | T_max _ | T_le _) ->
    let t1 = simplify_size_constraints t1 in
    let t2 = simplify_size_constraints t2 in
    (match t1, t2 with
    | T_size n, T_size m -> 
        if n <> m then raise (CannotUnify (t1,t2,loc))
    | T_size n, T_add(T_size m,a)
    | T_add(T_size m,a), T_size n ->
        if n >= m then
          unify ~loc (T_size (n-m)) a
        else raise (CannotUnify (t1,t2,loc))
    | T_infinity, T_size _ | T_size _,T_infinity -> raise (CannotUnify (t1,t2,loc))
    | _ -> ()) 
    | _ -> raise (CannotUnify (t1,t2,loc))


let rec ty_bindings ~loc p t = match p,canon t with
  | P_unit,T_const TUnit -> SMap.empty
  | P_var x,t -> SMap.singleton x t
  | P_tuple ps,T_tuple ts ->
      List.fold_left2 (fun m p t -> ty_bindings ~loc p t ++ m) SMap.empty ps ts
  | P_unit,t ->
      unify ~loc t (T_const TUnit);
      ty_bindings ~loc p t
  | P_tuple ps,t ->
      unify ~loc t (T_tuple (List.map (fun _ -> unknown ()) ps)); 
      ty_bindings ~loc p t

let tint n = T_const (TInt n)
let tbool = T_const TBool
let tunit = T_const TUnit

let initial_typing_env = SMap.empty

let env_extend ~loc ?(gen=false) g p scm = (* scm: scheme or type ?? *)
  g ++ SMap.map (fun t -> 
                   let scm = if gen then generalize (SMap.bindings g) t 
                              else Forall(Vs.empty,t) in
                 scm) (ty_bindings ~loc p scm)

exception UnboundVariable of x * Prelude.loc

let typ_ident g x loc =
  match SMap.find_opt x g with
  | None -> raise (UnboundVariable (x,loc))
  | Some t -> instance t

let fun_ty t1 n t2 =
  T_fun { arg = t1;
          dur = n;
          ret = t2 }

let ty_op ~loc = function
| Abs ->
    let tz = unknown() in
    fun_ty (tint tz) (T_size 0) (tint tz)
| Add|Sub|Mult|Div|Mod -> 
    let tz1 = unknown() in
    fun_ty (T_tuple[tint tz1;tint tz1]) (T_size 0) (tint tz1)
| Lt|Gt|Le|Ge|Eq|Neq -> 
    let tz1 = unknown() in
    fun_ty (T_tuple[tint tz1;tint tz1]) (T_size 0) tbool
| Not ->
    fun_ty tbool (T_size 0) tbool
| And|Or ->
    fun_ty (T_tuple[tbool;tbool]) (T_size 0) tbool
| Print -> 
    fun_ty (unknown()) (T_size 0) tunit
| To_string ->
    fun_ty (unknown()) (T_size 0) (T_string (unknown()))
| Random ->
    fun_ty (tint (T_size 32)) T_infinity (tint (T_size 32))
| Assert ->
    fun_ty tbool (T_size 0) tunit
| Wait n ->
    let v = unknown () in
    fun_ty v (T_size n) v
| TyConstr ty -> 
   let v = unknown() in
   unify ~loc ty v;
   fun_ty ty (T_size 0) v
| Buffer_lit n -> (* TODO *) assert false
| Buffer_make tz ->
   let v = unknown () in
   fun_ty v (T_size 0) (T_buffer{elem=v;size=tz})
| Buffer_get ->
  let v = unknown () in
  let tz_int = unknown () in
  fun_ty (T_tuple[T_buffer{elem=v;size=unknown ()};tint tz_int]) (T_size 0) v
| Buffer_update ->
  let v = unknown () in
  let vb = T_buffer{elem=v;size=unknown ()} in
  let tz_int = unknown () in
  fun_ty (T_tuple[vb;tint tz_int;v]) (T_size 0) vb
| Buffer_length ->
  let v = unknown () in
  let tz_int = unknown () in
  fun_ty (T_buffer{elem=v;size=unknown ()}) (T_size 0) (tint tz_int)
| String_get ->
  let tz_int = unknown () in
  fun_ty (T_tuple[T_string (unknown ());tint tz_int]) (T_size 0) (tint (T_size 8))
| String_length ->
  let tz_int = unknown () in
  fun_ty (T_string(unknown ())) (T_size 0) (tint tz_int)
| GetTuple{pos;arity} -> 
    let ts = List.init arity (fun _ -> unknown ()) in
    assert (0 <= pos && pos <= arity);
    fun_ty (group_ts ts) (T_size 0) (List.nth ts pos)

let ty_extern ext =
  let v = unknown() in
  match ext with
  | Array_make ->
      fun_ty (T_tuple[tint (T_size 32);v]) T_infinity (T_array v)
  | Array_get -> 
      fun_ty (T_tuple[T_array v;tint (T_size 32)]) T_infinity v
  | Array_set -> 
      fun_ty (T_tuple[T_array v;tint (T_size 32);v]) T_infinity tunit
  | Array_length ->
      fun_ty (T_array v) T_infinity (tint (T_size 32))

let typ_const ~loc = function
| Int _ -> (* TODO, add a type constraint according to the size of the literal *)
    let tz = unknown () in 
    tint tz
| Bool _ -> tbool
| Unit -> tunit
| String s -> T_string (T_size (String.length s))
| Op op -> ty_op ~loc op
| External ext -> ty_extern ext
| (V_loc _) -> 
    (* not in source program: handled in the typer *)
    unknown()

let rec non_expansive = function
  | E_deco(e,_) -> non_expansive e
  | E_app(e1,_) ->
      (match un_deco e1 with
      | E_const _ -> true
      | _ -> false)
  | E_if(e1,e2,e3) ->
      non_expansive e1 && non_expansive e2 && non_expansive e3
  | E_tuple es ->
      List.for_all non_expansive es
  | E_letIn(_,e1,e2) ->
      non_expansive e1 && non_expansive e2
  | _ -> true

exception Functional
let rec contain_fun t =
  match t with
  | T_const _ -> ()
  | T_var{contents=Unknown _} -> ()
  | T_var{contents=Ty t} -> contain_fun t
  | T_tuple (ts) ->
      List.iter contain_fun ts
  | T_fun _ ->
      raise Functional
  | T_array t ->
      contain_fun t
  | T_string _ -> ()
  | T_buffer {elem=t;size=tz} -> contain_fun t
  | (T_size _ | T_infinity | T_add _ | T_max _ | T_le _) -> ()

let check_conditional_shape ~loc e t =
  try contain_fun t with
  | Functional ->
      let open Prelude.Errors in
      error ~loc (fun fmt ->
      Format.fprintf fmt 
        "@[<v>expression %a has type %a.@,Conditional cannot have a type\
        \ containing functional types.@,Note: it is a current limitation\
        \ of the compiler.@]" (* Hint: use eta-expansion. *)
                 (emph_pp purple Ast_pprint.pp_exp) e
                 (emph_pp green pp_ty) t)



let check_app_shape ~loc e tret =
  match e with
  | E_app(E_const(Op (TyConstr _)),_) -> ()
  | _ ->  
    try contain_fun tret with
    | Functional ->
        let open Prelude.Errors in
        error ~loc (fun fmt ->
        Format.fprintf fmt 
          "@[<v>expression %a has type %a.@,Application cannot have a type\
          \ containing functional types.@,Note: it is a current limitation\
          \ of the compiler.@]"
                   (emph_pp purple Ast_pprint.pp_exp) e
                   (emph_pp green pp_ty) tret)


(* Subtyping_relation *)
module Response_time = struct
  let zero = T_size 0
  let one = T_size 1
  let infinity = T_infinity
  let max n m = T_max (n,m)
  let add n m = T_add (n,m)
end 

let is_TyConstr = function 
| E_const(Op(TyConstr _)) -> true 
| _ -> false 

let trace_last_exp = ref (E_const Unit) (* fake *)

let rec typ_exp ~toplevel ~loc (g:env) e =
  trace_last_exp := e;
  match e with
  | E_deco(e,loc) -> typ_exp ~toplevel ~loc g e
  | E_const c ->
      (typ_const ~loc c, Response_time.zero)
  | E_var(x) ->
      (* lookup *)
      (typ_ident g x loc, Response_time.zero)
  | E_if(e1,e2,e3) ->    
      let t1,n1 = typ_exp ~toplevel:false ~loc g e1 in
      let t2,n2 = typ_exp ~toplevel:false ~loc g e2
      and t3,n3 = typ_exp ~toplevel:false ~loc g e3 in
      unify ~loc t1 tbool;
      unify ~loc t2 t3;
      (** NB: [t2 <= t3] (according to the subtyping relation).
          So the conditional has type [t3] (that is not [t2] !) 
      *)
      let t = t3 in
      check_conditional_shape ~loc e t;
      (t,Response_time.(add n1 (max n2 n3)))
  | E_tuple(es) ->
      let ts,ns = List.split @@ List.map (typ_exp ~toplevel:false ~loc g) es in
      let n = List.fold_left Response_time.add Response_time.zero ns in
      T_tuple ts,n
  | E_fun(p,e1) ->
      let v = unknown() in
      let g' = env_extend ~loc g p v in
      let t,dur = typ_exp ~toplevel:false ~loc g' e1 in
      (T_fun{arg=v;dur;ret=t}, Response_time.zero)
  | E_app(e1,e2) -> 
      let t1,n1 = typ_exp ~toplevel:false ~loc g e1 in
      let t2,n2 = typ_exp ~toplevel:(toplevel && is_TyConstr e1) ~loc g e2 in
      let t = unknown () in
      let n = unknown () in
      unify ~loc:(loc_of e1) (T_fun{arg=t2;dur=n;ret=t}) t1; (* t1 in second for subtyping *)
      check_app_shape ~loc e t;
      (t, Response_time.(add n (add n1 n2)))
  | E_letIn(p,e1,e2) ->
      let t1,n1 = typ_exp ~toplevel:false ~loc g e1 in
      let gen = non_expansive e1 in
      let g' = env_extend ~loc ~gen g p t1 in
      (if not toplevel then () else
       begin
        let open Prelude.Errors in
        let open Format in
        if not (evaluated e1 || is_variable e1) then begin
          error ~loc (fun fmt ->
            fprintf fmt "Toplevel declarations like";
            let xs = vars_of_p p in
            pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt ",")
                (fun fmt (x,_) -> fprintf fmt " %s" x) fmt (SMap.bindings xs);
            fprintf fmt " should be values or variables."
        ) end;
        fprintf std_formatter "val %a : %a\n" Ast_pprint.pp_pat p pp_ty (canon t1);
       
       end);

      let t2,n2 = typ_exp ~toplevel ~loc g' e2 in

      (t2, Response_time.add n1 n2)
  | E_fix(f,(x,e1)) -> 
      let t1 = unknown () in
      let t2 = unknown () in
      let tf = T_fun{ arg = t1;
                      dur = Response_time.(add one (unknown()));
                      ret = t2 } in 
      let g' = env_extend ~loc g (P_var f) tf in
      let t,n = typ_exp ~toplevel:false ~loc g' (E_fun(x,e1)) in
      (** NB: [t <= tf] (according to the subtyping relation) *)
      (* unify ~loc t tf; *)
      (tf, n)
  | E_reg(V ev,e0) ->
     let tv,nv = typ_exp ~toplevel:false ~loc g ev in
     let t0,n0 = typ_exp ~toplevel:false ~loc g e0 in
     unify ~loc tv (T_fun{ arg = t0;
                           dur = Response_time.zero;
                           ret = t0 }) ;
     unify ~loc nv Response_time.zero;
     unify ~loc n0 Response_time.zero;
     (t0, Response_time.zero)
  | E_exec(e1,e2,_) ->
     let t1,_ = typ_exp ~toplevel:false ~loc g e1 in
     let t2,n2 = typ_exp ~toplevel:false ~loc g e2 in
     unify ~loc t1 t2;
     unify ~loc n2 Response_time.zero;
     (T_tuple[t1;tbool], Response_time.zero)
  | E_par(e1,e2) ->
    let t1,n1 = typ_exp ~toplevel:false ~loc g e1 in
    let t2,n2 = typ_exp ~toplevel:false ~loc g e2 in
    T_tuple [t1;t2],T_max(n1,n2)
  | E_set (x,e1) ->
     let t1,n = typ_exp ~toplevel:false ~loc g e1 in
     unify ~loc n Response_time.zero;
     let t2 = typ_ident g x loc in
     unify ~loc t1 t2;
     (t1, Response_time.zero)
  | E_step(e1,_) -> 
      let t1,_ = typ_exp ~toplevel:false ~loc g e1 in
      unify ~loc t1 (T_const TUnit);
      (t1, Response_time.zero)
 | E_lastIn(x,e1,e2) ->
      let t1,n1 = typ_exp ~toplevel:false ~loc g e1 in
      let g' = env_extend ~loc g (P_var x) t1 in
      unify ~loc n1 Response_time.zero;
      let t2,n2 = typ_exp ~toplevel:false ~loc g' e2 in
      unify ~loc n2 Response_time.zero;
      (t2, Response_time.zero)

let typing_handler ?(msg="") f () =
  let open Prelude.Errors in
  try 
    f ()
  with CannotUnify(t1,t2,loc) ->   
    error ~loc (fun fmt ->
      Format.fprintf fmt "%s@,An expression has type %a but was expected of type %a"
                 msg
                (* (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp *)
                 (emph_pp bold pp_ty) t1 
                 (emph_pp bold pp_ty) t2)
  | Cyclic(n,t,loc) ->
      error ~loc (fun fmt ->
      Format.fprintf fmt "%s@,expression %a has a cyclic type %a\n" 
          msg  (emph_pp purple Ast_pprint.pp_exp) !trace_last_exp
                 (emph_pp bold pp_ty) t)
  | UnboundVariable(x,loc) ->
      Prelude.Errors.raise_error ~loc ~msg:("unbound variable "^x) ()


let typing ?(env=SMap.empty) ?(msg="") e =
  typing_handler (fun () ->
    let t,n = typ_exp ~toplevel:true ~loc:(loc_of e) env e in
    canon t, simplify_size_constraints n) ()


(** [fun_shape ty] returns a type [ty -{'a}-> 'b] 
    where ['a] and ['b] are fresh type variable. *)
let fun_shape (t_arg : ty) : ty = 
  fun_ty t_arg
         (unknown()) 
         (unknown())

(** Typing of the program [e]. 
   The program must be a function of type [t1 -> t2] that takes 
   an input flow [arg_list], each element of the flow being of type [t1].

   Returns the type of [e] and a type abstraction of its reponse time.
 *)
let typing_with_argument (e : e) (arg_list : e list) : ty * ty =

  let t_arg = unknown() in
  let (ty,response_time) = typing (mk_loc (loc_of e) @@ ty_annot ~ty:(fun_shape t_arg) e) in

  List.iter (fun a -> typing ~msg:"checking inputs given by option -arg, "
                         (ty_annot ~ty:t_arg a) 
                      |> ignore) arg_list;

  (ty, response_time)


let when_repl : ((p * e) * Prelude.loc) -> unit = 
  let r = ref SMap.empty in
  fun ((p,e),loc) ->
    let (ty,_) = typing ~env:!r e in
    let gen = non_expansive e in
    r := typing_handler (fun () -> (env_extend ~loc ~gen !r p ty)) ();
    Format.fprintf Format.std_formatter "val %a : %a@."  Ast_pprint.pp_pat p pp_ty ty
