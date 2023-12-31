%{
  
  open Prelude  
  open Ast
  open Ast_mk

  (* location augmented we a file name *)
  let with_file loc =
    (!Current_filename.current_file_name, loc)
%}

%token LPAREN RPAREN LBRACKET RBRACKET COMMA PIPE_PIPE EQ EQ_EQ COL SEMI
%token SHARP_PIPE_LBRACKET PIPE_RBRACKET
%token FUN AMP DOT REGISTER EXEC LAST DEFAULT
%token NODE IMPLY
%token LET REC AND IN IF THEN ELSE FIX
%token <string> IDENT TVAR_IDENT
%token <bool> BOOL_LIT
%token <int> INT_LIT
%token PLUS MINUS TIMES LT LE GT GE NEQ NOT MOD DIV AMP_AMP OR
%token EOF
%token SEMI_SEMI
%token LEFT_ARROW RIGHT_ARROW
%token EXIT_REPL
%token <string> STRING_LIT

/* The precedences must be listed from low to high. */

%nonassoc IN
%nonassoc SEMICOL
%nonassoc LET
%left     COMMA
%nonassoc IF THEN ELSE
%right    AMP_AMP OR
%left     LT LE GT GE NEQ EQ
%left     PLUS MINUS
%left     TIMES
%right    DIV MOD
%nonassoc DOT
%nonassoc BOOL_LIT IDENT LPAREN

%start <((p*e)*Prelude.loc) list> pi
%start <((p*e)*Prelude.loc) option> decl_opt
%start <e> exp_eof

%%

pi:
| ds=pi_desc { ds }

exp_eof:
| e=exp EOF {e}

pi_desc:
| ds=decl* EOF 
       { ds }

decl_opt:
| d=decl { Some d } 
| EXIT_REPL { None }

decl:
| LET b=after_let(SEMI_SEMI)
        { b,(with_file $loc) }
| NODE b=fun_decl(SEMI_SEMI)
        { enforce_node b,(with_file $loc) } 

| e=exp SEMI_SEMI { ((P_var "_", e),(with_file $loc))  }
/*
| EOF { E_var (!Ast_mk.main_symbol) }*/


fun_decl(In_kw):
| f=IDENT p_ty_opt=arg_ty_atomic 
                  ty_opt_ret=ret_ty_annot_eq 
    e1=exp In_kw
        { 
            let ef = mk_let_fun ~loc:(with_file ($startpos(f),$endpos(e1)))
                                ~p_ty_opt 
                                ~ty_opt_ret
                                e1 
            in
            (P_var f,ef)
        }


after_let(In_kw):
| b=bindings(apat,exp) In_kw { b }
| b=fun_decl(In_kw)
        { b }
| e=fun_rec_decl(In_kw) 
        { e }
| REC f_ty_opt=ty_annot(IDENT) EQ e1=exp In_kw
        {  
            let f,ty_opt = f_ty_opt in
            let loc_fun = with_file ($startpos(f_ty_opt),$endpos(e1)) in
            P_var f, mk_fix f (ty_annot_opt ~ty:ty_opt e1) loc_fun 
        }
fun_rec_decl(In_kw):
| REC f=IDENT p_ty_opt=arg_ty_atomic ty_opt=ret_ty_annot_eq e1=exp In_kw
        { 
            let p_ty_opt_f = 
              let open Typing in
              match p_ty_opt with
              | p,None -> p,None
              | p,Some t -> p,Some (fun_ty t (unknown()) (unknown()))
            in 
            let loc_fun = with_file ($startpos(f),$endpos(e1)) in
            let (p,ty_f_opt) = p_ty_opt_f in
            let ef = mk_fun_ty_annot p ty_f_opt (ty_annot_opt ty_opt e1) 
                   |> mk_loc loc_fun in 
            P_var f, mk_fix f ef loc_fun 
        }




ty_annot(X) :
| x=X   
        { x,None }
| x=X COL ty=ty
        { x,Some ty }
| LPAREN x_ty_opt=ty_annot(X) RPAREN  
        { x_ty_opt }

ty:
| arg=oty IMPLY ret=oty { T_fun{arg;dur=T_size 0;ret} }
| arg=oty RIGHT_ARROW ret=oty { T_fun{arg;dur=(Typing.unknown());ret} }
| arg=oty MINUS LBRACKET ty=ty RBRACKET RIGHT_ARROW ret=oty { T_fun{arg;dur=ty;ret} }
| t=oty { t }

oty:
| tys=separated_nonempty_list(TIMES,aty) { group_ts tys }

aty:
| x=IDENT { T_const (match x with 
                     | "unit" -> TUnit
                     | "bool" -> TBool
                     | "int" -> TInt (T_size 32)
                     | s -> Prelude.Errors.raise_error ~loc:(with_file $loc) ~msg:("unbound type constructor "^s) ()) }
| x=IDENT LT tz=ty GT { match x with 
                        | "string" -> T_string tz
                        | "int" -> T_const (TInt tz)
                        | s -> Prelude.Errors.raise_error ~loc:(with_file $loc) ~msg:("unbound type constructor "^s) () }
| at=aty x=IDENT LT tz=ty GT { match x with 
                        | "buffer" -> T_buffer{elem=at;size=tz}
                        | s -> Prelude.Errors.raise_error ~loc:(with_file $loc) ~msg:("unbound type constructor "^s) () }
| n=INT_LIT           { T_size n }
| x=TVAR_IDENT { Typing.unknown () } /* TODO: hashmap to constraints occurences */
| LPAREN ty=ty RPAREN { ty }

value:
| e=lvalue COMMA es=separated_nonempty_list(COMMA,lvalue) 
    { E_tuple (e::es) }
| e=lvalue { e }

lvalue:
| v=avalue { v }
| FUN p_ty_opt=arg_ty RIGHT_ARROW e=exp  
      { 
        let p,ty_opt = p_ty_opt in
        mk_fun_ty_annot p ty_opt e 
    }

avalue:
| LPAREN e=value RPAREN { e }
| c=const { E_const c }


/* decorated expression */
exp:
  e=exp_desc { mk_loc (with_file $loc) e }

/* expression description
   with decorated sub-expression */
exp_desc:
| e1=app_exp SEMI e2=exp
        { 
            E_letIn(P_unit,e1,e2)
        }
| e=lexp COMMA es=separated_nonempty_list(COMMA,lexp)
        { 
            E_tuple (e::es) 
        }
| e1=lexp PIPE_PIPE e2=lexp
        { 
            E_par(e1,e2)
        }
| e=lexp {e}


arg_ty_unparen:
| p=pat { p, None }
| p=apat COL ty=aty  {p, Some ty}

arg_ty:
| a=arg_ty_unparen
| LPAREN a=arg_ty_unparen RPAREN { a }

arg_ty_atomic:
| LPAREN p=pat RPAREN { p, None }
| LPAREN p=apat COL ty=ty RPAREN {p, Some ty}
| p=apat { p, None }


lexp: 
  e=lexp_desc { mk_loc (with_file $loc) e }

lexp_desc:
| e=app_exp { e }
| FIX f=IDENT e=exp
        { mk_fix f e (with_file $loc) } 
| FUN p_ty_opt=arg_ty RIGHT_ARROW e=exp 
        { let (p,ty_p_opt) = p_ty_opt in
          mk_fun_ty_annot_p p ty_p_opt e }
| IF e1=exp THEN e2=lexp ELSE e3=exp 
        { E_if(e1,e2,e3) }
| LET b=after_let(IN) e2=exp
        { let (p,e1) = b in 
          E_letIn(p,e1,e2) }
| NODE b=fun_decl(IN) e2=exp
        { let (p,e1) = enforce_node b in 
          E_letIn(p,e1,e2) }

ret_ty_annot_eq:
| EQ { None }
| COL ty=ty EQ { Some ty }

bindings(P,E):
| b=binding(P,E) { b }
| b=binding(P,E) AND bs=separated_nonempty_list(AND,binding(P,E)) 
  { let ps,es = List.split (b::bs) in 
    group_ps ps, group_es es }

binding(P,E):
| p_ty_opt=ty_annot(P) EQ e=E 
        { 
            let p,ty_opt = p_ty_opt in 
            p,ty_annot_opt ~ty:ty_opt e 
        }
| p=P EQ e=E 
    { p,e }

app_exp: 
  e=app_exp_desc { mk_loc (with_file $loc) e }

app_exp_desc:
| e1=aexp e2=aexp { E_app(e1,e2) }
| MINUS e1=aexp { E_app(E_const(Op(Sub)),E_tuple[E_const(Int(0,Typing.unknown()));e1]) }
| e1=app_exp op=binop e2=app_exp 
        { E_app (mk_loc (with_file $loc) @@ E_const (Op op),
                 mk_loc (with_file $loc) @@ E_tuple [e1;e2]) 
        }
| e1=app_exp AMP_AMP e2=app_exp  
        { let e3 = mk_loc (with_file $loc) @@ E_const (Bool false) in
          E_if(e1,e2,e3) 
        }
| e1=app_exp OR e3=app_exp 
        { let e2 = mk_loc (with_file $loc) @@ E_const (Bool true) in
          E_if(e1,e2,e3)
        }
| REGISTER ev=avalue LAST e0=aexp 
       {
         E_reg(V ev,e0) 
       }
| REGISTER f=IDENT LAST e0=aexp 
       { 
        E_reg (V (E_var f),e0)
        (* let y = gensym () in
         E_reg(V (E_fun(y,E_app(E_var f,E_var y))),e0) *)
       }
| EXEC e1=exp DEFAULT e2=lexp 
       { E_exec(e1,e2,"") }
| e1=aexp DOT LPAREN e2=exp RPAREN
       { E_app(mk_loc (with_file $loc) @@ E_const (External Array_get),
               mk_loc (with_file $loc) @@ E_tuple[e1;e2]) }
| e1=aexp DOT LPAREN e2=exp RPAREN LEFT_ARROW e3=app_exp
    { E_app(mk_loc (with_file $loc) @@ E_const (External Array_set),
            mk_loc (with_file ($startpos(e1),$endpos(e2))) @@ E_tuple[e1;e2;e3]) }
| e=aexp { e }


aexp: 
  e=aexp_desc { mk_loc (with_file $loc) e }

aexp_desc:
| LPAREN e=exp RPAREN { e }
| LPAREN e=exp COL ty=ty RPAREN { ty_annot ~ty e }
| c=const { E_const c }
| x=IDENT { match x with 
            | "abs" -> E_const (Op Abs)
            | "print" -> E_const (Op Print)
            | "to_string" -> E_const (Op To_string)
            | "buffer_make" -> E_const (Op (Buffer_make (Typing.unknown ())))
            | "buffer_get" -> E_const (Op Buffer_get)
            | "buffer_update" -> E_const (Op Buffer_update)
            | "buffer_length" -> E_const (Op Buffer_length)
            | "string_get" -> E_const (Op String_get)
            | "string_length" -> E_const (Op String_length)
            | "random" -> E_const (Op Random)
            | "assert" -> E_const (Op Assert)
            | "array_make" -> E_const (External Array_make)
            | "array_length" -> E_const (External Array_length)
            | _ -> E_var x }
| SHARP_PIPE_LBRACKET separated_list(COMMA,app_exp) PIPE_RBRACKET
    { (* Buffer n *) assert false (*todo*)  }

pat:
| p=apat { p }
| p=apat COMMA ps=separated_nonempty_list(COMMA,apat)
  { P_tuple (p::ps) }

apat:
| LPAREN RPAREN { P_unit }
| LPAREN p=pat RPAREN { p }
| x=IDENT { P_var x }

const:
| LPAREN RPAREN { Unit }
| b=BOOL_LIT { Bool b }
| n=INT_LIT  { 
    Int (n,Typing.unknown()) }
| s=STRING_LIT  { String s }
| NOT { Op Not }
| LPAREN op=binop RPAREN { Op op }


%inline binop:
| PLUS       { Add }
| MINUS      { Sub }
| TIMES      { Mult }
| DIV        { Div }
| MOD        { Mod }
| LT         { Lt }
| GT         { Gt }
| LE         { Le }
| GE         { Ge }
| EQ | EQ_EQ { Eq }
| NEQ        { Neq }
| AMP        { And }
| OR         { Or }
