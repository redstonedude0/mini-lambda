/* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the grammar of the language, with
 * the productions builtin the Abstract Syntax Tree.
 */

%{
open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token PLUS
%token EQUALS
%token NEQUALS
%token OR2
%token AND2
%token MINUS
%token LPAREN RPAREN LBRACE RBRACE
%token IF
%token ELSE
%token WHILE
%token BREAK
%token CONTINUE
%token COLON
%token FUNC
%token FOR
%token RETURN
%token ARROW
%token LAMBDA
%token BIND
%token COMMA
%token SEMI
%token EOF

%start program
%type <Ast.program> program

%%

program:
  funcs = list(func) EOF { Array.of_list funcs }


func:
  | FUNC name = IDENT;
    LPAREN params = separated_list(COMMA, IDENT); RPAREN
    body = func_body
    { { name; params; body; loc = $startpos } }

func_body:
  | LBRACE body = statements; RBRACE { Some(body) }
  | SEMI { None }

block:
  | LBRACE body = statements; RBRACE { body }

statements:
  | statement statements { $1 :: $2 }
  | multistatement statements { $1 @ $2 }
  | { [] }

multistatement:
  | FOR LPAREN s_init=statement; e_test=expr SEMI s_incr=statement RPAREN body=block { s_init :: [WhileStmt($startpos,e_test,body @ [s_incr],None)] }

statement:
  | RETURN expr SEMI { ReturnStmt($startpos, $2) }
  | IDENT BIND expr SEMI { BindStmt($startpos, $1, $3) }
  | expr SEMI { ExprStmt($startpos, $1) }
/* For now not using optional() menhir construct because it (should) produce the same AST either way */
  | IF body1=expr body2=block ELSE body3=block { IfStmt($startpos, body1, body2, Some(body3)) }
  | IF body1=expr body2=block { IfStmt($startpos, body1, body2, None) }
  | WHILE body1=expr body2=block { WhileStmt($startpos, body1, body2, None) }
  | label=IDENT COLON WHILE body1=expr body2=block { WhileStmt($startpos, body1, body2, Some(label)) }
  | BREAK SEMI { BreakStmt($startpos, None) }
  | BREAK label=IDENT SEMI { BreakStmt($startpos, Some(label)) }
  | CONTINUE SEMI { ContinueStmt($startpos, None) }
  | CONTINUE label=IDENT SEMI { ContinueStmt($startpos, Some(label)) }

expr:
  | unary_expr { $1 }
  | lhs = expr; PLUS; rhs = unary_expr
    { AddExpr($startpos, lhs, rhs) }
  | lhs = expr; MINUS; rhs = unary_expr
    { SubtractExpr($startpos, lhs, rhs) }
  | lhs = expr; EQUALS; rhs = unary_expr
    { EqualsExpr($startpos ,lhs, rhs) }
  | lhs = expr; NEQUALS; rhs = unary_expr
    { NequalsExpr($startpos ,lhs, rhs) }
  | lhs = expr; OR2; rhs = unary_expr
    { Or2Expr($startpos ,lhs, rhs) }
  | lhs = expr; AND2; rhs = unary_expr
    { And2Expr($startpos ,lhs, rhs) }

unary_expr:
  | LAMBDA
    LPAREN params = separated_list(COMMA, IDENT); RPAREN
    ARROW
    body = postfix_expr;
    { LambdaExpr($startpos, params, body) }
  | postfix_expr { $1 }

postfix_expr:
  | primary_expr { $1 }
  | callee = primary_expr; LPAREN args = separated_list(COMMA, expr); RPAREN
    { CallExpr($startpos, callee, args) }

primary_expr:
  | LPAREN e = expr; RPAREN { e }
  | name = IDENT { IdentExpr($startpos, name) }
  | decimal = INT { IntExpr($startpos, decimal) }
  | boolean = BOOL { BoolExpr($startpos, boolean) }


