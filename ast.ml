(* Compntler Construction - Minimal Lambda Language
 *
 * This file defined the Abstract Syntax Tree, produced
 * by the parser. In order to generate accurate error
 * messages further down the pipeline, source locations
 * are included in each node.
 *)

type loc = Lexing.position

type expr
  = IdentExpr of loc * string
  | IntExpr of loc * int
  | BoolExpr of loc * bool
  | AddExpr of loc * expr * expr
  | EqualsExpr of loc * expr * expr
  | NequalsExpr of loc * expr * expr
  | Or2Expr of loc * expr * expr
  | And2Expr of loc * expr * expr
  | SubtractExpr of loc * expr * expr
  | LambdaExpr of loc * string list * expr
  | CallExpr of loc * expr * expr list

type statement
  = ReturnStmt of loc * expr
  | ExprStmt of loc * expr
  | BindStmt of loc * string * expr

type func =
  { name: string
  ; params: string list
  ; body: (statement list) option
  ; loc: loc
  }

type program = func array

