open Combinatorial
open Fsm_syntax

let allow_heap_access = ref false
let allow_heap_assign = ref false

module SMap = Map.Make(String)
module IMap = Map.Make(Int)

let (+++) s1 s2 =  IMap.union (fun _ _ v2 -> Some v2) s1 s2
let (++>) s1 s2 =  SMap.union (fun _ s1 s2 -> Some (s1 +++ s2)) s1 s2

let mk_int n size = 
  assert (n >= 0 && size > 0);
  A_const (Int {value=n;tsize=TSize size})

let new_instance = 
  let c  =ref 0 in
  (fun () -> incr c; !c)

let to_c = function
| Ast.Unit -> Unit
| Ast.Int (n,tz) -> Int {value=n;tsize=Fsm_typing.translate_ty tz}
| Ast.Bool b -> Bool b
| Ast.String s -> String s
| Ast.(Op _ | External _ | V_loc _) -> assert false

let to_op = function
| Ast.TyConstr ty -> TyConstr (Fsm_typing.translate_ty ty)
| Ast.Add -> Add
| Ast.Sub -> Sub
| Ast.Mult -> Mult
| Ast.Div -> Div
| Ast.Mod -> Mod
| Ast.Lt -> Lt
| Ast.Le -> Le
| Ast.Gt -> Gt
| Ast.Ge -> Ge
| Ast.Eq -> Eq
| Ast.Neq -> Neq
| Ast.And -> And
| Ast.Or -> Or
| Ast.Not -> Not
| Ast.GetTuple {pos=i;arity=n} -> GetTuple (i,n,new_tvar())
| Ast.Print -> assert false
| Ast.To_string -> To_string
| Ast.String_length -> String_length (new_tvar())
| Ast.String_get -> assert false
| Ast.(Assert|Abs|Random|Buffer_get|Buffer_update|Buffer_length|Wait _|
Buffer_lit _|Buffer_make _) -> assert false

let rec to_a = function
| Ast.E_var x -> A_var x
| Ast.E_const c -> A_const (to_c c)
| Ast.E_app(E_const(Op String_get),e) ->
    let z = Ast.gensym () in
    let sx = Ast.gensym () in
    let ix = Ast.gensym () in 
    A_letIn(z,to_a e,
    A_letIn(sx, A_call(GetTuple (0,2,new_tvar()),A_var z),
    A_letIn(ix, A_call(GetTuple (1,2,new_tvar()),A_var z),
    A_string_get(sx,ix))))
| Ast.E_app(E_const(Op op),e) -> 
    let x = Ast.gensym () in
    A_letIn(x,to_a e, A_call(to_op op,A_var x))
| Ast.E_if(e1,e2,e3) -> A_call(If,A_tuple [to_a e1;to_a e2;to_a e3])
| Ast.E_tuple(es) -> A_tuple (List.map to_a es)
| Ast.E_letIn(P_var x,e1,e2) -> A_letIn(x,to_a e1,to_a e2)
| e -> Format.fprintf Format.std_formatter "--> %a\n"  Ast_pprint.pp_exp  e; assert false

let replace_arg e =
  match e with
  | Ast.E_fix(f,(P_var x,e1)) -> Ast_subst.subst_e x (E_var (Naming_convention.formal_param_of_fun f)) e1
  | _ -> assert false

let access ~ks ~result (address:a) ~field =
  allow_heap_access := true;
  let q = Ast.gensym ~prefix:"wait_read" () in
  let t = q, (S_if(A_call(Not,A_var ("avm_rm_waitrequest")), 
                  (seq_ (S_set(Delayed,"avm_rm_read",A_const (Bool false))) @@
                   seq_ (S_set(Delayed,result,A_var "avm_rm_readdata")) @@ ks),
                   Some (S_continue (q,A_const Unit,None))))
  in
  let s = 
    seq_ (S_set(Delayed,"avm_rm_address",A_call(Compute_address,(A_tuple [address;field])))) @@
    seq_ (S_set(Delayed,"avm_rm_read",A_const (Bool true))) @@
    S_continue (q,A_const Unit,None)
  in
  (S_fsm(Ast.gensym ~prefix:"id" (),Ast.gensym ~prefix:"id" (),[t],s,true))


let assign ~ks ~result ?(value=A_const Unit) (address:a) ~field data =
  allow_heap_assign := true;
  let q = Ast.gensym ~prefix:"wait_write" () in
  let t = q, (S_if(A_call(Not,A_var ("avm_wm_waitrequest")), 
                  (seq_ (S_set(Delayed,"avm_wm_write",A_const (Bool false))) @@
                   seq_ (S_set(Delayed,result,value)) @@ ks),
                   Some (S_continue (q,A_const Unit,None))))
  in
  let s = 
    seq_ (S_set(Delayed,"avm_wm_address",A_call(Compute_address,(A_tuple [address;field])))) @@
    seq_ (S_set(Delayed,"avm_rm_read",data)) @@
    seq_ (S_set(Delayed,"avm_rm_read",A_const (Bool true))) @@
    S_continue (q,A_const Unit,None)
  in
  (S_fsm(Ast.gensym ~prefix:"id" (),Ast.gensym ~prefix:"id" (),[t],s,true))
 

let rec to_s ~tail x ks e =
  if combinatorial e then 
  SMap.empty,S_letIn(x,to_a e,ks) else
  match e with
  | E_deco _ -> 
      Ast_undecorated.still_decorated e
  | E_fun _ | E_fix _ ->
   assert false  (* already globalized *)
  | E_const _ | E_var _ | E_tuple _ -> assert false (* already handled *)
  | E_app(E_var f,a) -> 
      if tail then SMap.empty, S_continue(f,to_a a,None) else
      let id = new_instance() in
      let w = SMap.singleton f (IMap.singleton id (x,ks)) in
      w, S_continue(f,to_a a,Some(id))
  | E_app(E_const (Op(Print)),e1) ->
      SMap.empty,seq_ (S_print(to_a e1)) ks
  | E_app(E_const (Op(To_string)),e1) ->
      SMap.empty,S_letIn(x,A_call(To_string,to_a e1),ks)
  
  | E_app(E_const (External(Array_get)),E_tuple[e1;e2]) ->
      let arr = to_a e1 in
      let idx = to_a e2 in
      SMap.empty,access ~ks ~result:x arr ~field:idx
  | E_app(E_const (External(Array_length)),e1) ->
      let arr = to_a e1 in
      SMap.empty,access ~ks ~result:x arr ~field:(A_call(Sub,A_tuple[mk_int 0 32;mk_int 1 32]))
  | E_app(E_const (External(Array_set)),E_tuple[e1;e2;e3]) ->
      let arr = to_a e1 in
      let idx = to_a e2 in
      let v = to_a e3 in
      SMap.empty,assign ~ks ~result:x arr ~field:idx v

  | E_app _ ->
      assert false (* computed functions should be eliminated before *)
  | E_letIn(P_var y,e1,e2) when combinatorial e1 -> 
      let w2,s2 = to_s ~tail x ks e2 in
      w2,S_letIn(y,to_a e1,s2)
  | E_letIn(P_var y,e1,e2) ->
     let w2,s2 = to_s ~tail:false x ks e2 in
     let w1,s1 = to_s ~tail y s2 e1 in
     w1 ++> w2, s1
  (* | E_letIn(P_tuple ps,e1,e2) ->
     let y = Ast.gensym () in
     let w2,s2 = to_s ~tail:false x ks e2 in
     let w1,s1 = to_s ~tail y s2 e1 in
     w1 ++> w2, s1*)
  | (E_letIn(P_tuple _,_,_) | E_letIn(P_unit,_,_)) -> assert false (* should be removed before *)
  | E_if(e,e1,e2) ->
     let w1,s1 = to_s ~tail(* :false  ?? *) x ks e1 in
     let w2,s2 = to_s ~tail x ks e2 in
     w1 ++> w2, S_if(to_a e,s1,Some s2)
  | E_lastIn(y,e1,e2) ->
     let w2,s2 = to_s ~tail x ks e2 in
     w2,seq_ (S_if (A_call(Not,A_var (y^"_init")),
                            seq_ (S_set(Immediate,y,to_a e1))
                                 (S_set(Immediate,y^"_init",A_const (Bool true))),
                            None)) s2
  | E_set(y,e1) ->
     SMap.empty,seq_ (S_set(Immediate,y,to_a e1)) ks
  | E_step(e1,k) ->
      let ds,e1' = Middle_end.compile e1 in
      let _,w,(ts,s) = compile ~result:k (ds,e1') in
      w,seq_ (S_fsm(Ast.gensym ~prefix:"id" (),k,ts,s,true)) ks
  | E_par(e1,e2) ->
      let ds1,e1' = Middle_end.compile e1 in
      let ds2,e2' = Middle_end.compile e2 in
      let z1 = Ast.gensym () in
      let z2 = Ast.gensym () in
      let _,w1,(ts1,s1) = compile ~result:z1 (ds1,e1') in
      let _,w2,(ts2,s2) = compile ~result:z2 (ds2,e2') in
      let id1 = Ast.gensym ~prefix:"id" () in
      let id2 = Ast.gensym ~prefix:"id" () in
      let s1 = S_if(A_call(Not,A_var (id1^"_started")), seq_ (S_set(Immediate,id1^"_started",A_const (Bool true))) @@ s1,None) in
      let s2 = S_if(A_call(Not,A_var (id2^"_started")), seq_ (S_set(Immediate,id2^"_started",A_const (Bool true))) @@ s2,None) in
      w1 ++> w2 , seq_ (S_fsm(id1,z1,ts1,s1,false)) @@
                  seq_ (S_fsm(id2,z2,ts2,s2,false)) @@
                  S_if(A_call(And,A_tuple[A_var (id1^"_rdy");A_var (id2^"_rdy")]), 
                      (seq_ (S_set(Immediate,x,(A_tuple[A_var z1;A_var z2]))) @@
                       seq_ (S_set(Immediate,id1^"_started",A_const (Bool false))) @@
                       seq_ (S_set(Immediate,id2^"_started",A_const (Bool false))) @@
                              ks),None)
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)


and to_s_top ~tail e = 
  let result = Ast.gensym () in
  to_s ~tail result (S_return (A_var result)) e


and insert_kont w (x,s) =
  match SMap.find_opt x w with
  | None -> Some(x,s)
  | Some u ->
  let n = IMap.cardinal u in
  let l = List.of_seq (IMap.to_seq u) in
  if n <= 0 then None else
  let rec aux = function
  | S_return a -> 
      S_case(A_var (Naming_convention.instance_id_of_fun x),
             (List.map (fun (n,(x,sn)) -> 
                         Enum (Naming_convention.instance_enum_const n),S_letIn(x,a,sn)
                ) l))
  | S_continue _ as s -> s
  | S_if(a,s1,so) -> S_if(a,aux s1,Option.map aux so)
  | S_case(a,hs) -> S_case(a,List.map (fun (c,s) -> c, aux s) hs)
  | S_set _ as s -> s
  | S_seq(s1,s2) ->  S_seq(aux s1,aux s2)
  | S_letIn(x,a,s) -> S_letIn(x,a,aux s)
  | S_fsm _ as s -> s (* ok? *)
  | S_print _ as s -> s
  in 
  Some(x,aux s)

(* takes a program composed of global recursive (non-mutual) 
   functions [ds] and an entry point [e] and translates it into a FSM *)
and to_prog (ds,e) =
  let rec aux w rts = function
  | [] -> let w',s = to_s_top ~tail:false e in
           (w' ++> w),(List.rev rts,s)
  | (x,e)::ds' ->
      let w',s = to_s_top ~tail:true e in
      aux (w' ++> w) ((x,s)::rts) ds'
  in 
  let w,(ts,s) = aux SMap.empty [] ds in
  let ts' = List.filter_map (insert_kont w) ts in
  (w,ts',s)


and compile ~result (ds,e) =
  let ds' = List.map (fun (x,e) -> x, replace_arg e) ds in
  let w,ts,s = to_prog (ds',e) in

  result,w,(ts,s)
