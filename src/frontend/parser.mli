
(* The type of tokens. *)

type token = 
  | TVAR_IDENT of (string)
  | TIMES
  | THEN
  | STRING_LIT of (string)
  | SHARP_PIPE_LBRACKET
  | SEMI_SEMI
  | SEMI
  | RPAREN
  | RIGHT_ARROW
  | REGISTER
  | REC
  | RBRACKET
  | PLUS
  | PIPE_RBRACKET
  | PIPE_PIPE
  | OR
  | NOT
  | NODE
  | NEQ
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LET
  | LEFT_ARROW
  | LE
  | LBRACKET
  | LAST
  | INT_LIT of (int)
  | IN
  | IMPLY
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FUN
  | FIX
  | EXIT_REPL
  | EXEC
  | EQ_EQ
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | DEFAULT
  | COMMA
  | COL
  | BOOL_LIT of (bool)
  | AND
  | AMP_AMP
  | AMP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val pi: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (((Ast.p * Ast.e) * Prelude.loc) list)

val exp_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.e)

val decl_opt: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (((Ast.p * Ast.e) * Prelude.loc) option)
