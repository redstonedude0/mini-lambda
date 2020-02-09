(* Compiler Construction - Minimal Lambda Language
 *
 * This is a version of the AST produced by the type checker.
 * It does not actually include type information presently
 * since it is not needed by other passes. Lambda captures
 * are the targets of named references are made explicit here.
 *)

type loc = Lexing.position

type id = int

type expr
  = FuncExpr of loc * id
  | EnvExpr of loc * id
  | BoundExpr of loc * id
  | ArgExpr of loc * id
  | IntExpr of loc * int
  | BoolExpr of loc * bool
  | AddExpr of loc * expr * expr
  | EqualsExpr of loc * expr * expr
  | NequalsExpr of loc * expr * expr
  | Or2Expr of loc * expr * expr
  | And2Expr of loc * expr * expr
  | SubtractExpr of loc * expr * expr
  | LambdaExpr of loc * int * expr array * expr
  | CallExpr of loc * expr * expr array

type statement
  = ReturnStmt of loc * expr
  | ExprStmt of loc * expr
  | BindStmt of loc * id * expr
  | AssignStmt of loc * id * expr
  | IfStmt of loc * id * expr * (statement list) * ((statement list) option)
  | WhileStmt of loc * expr * (statement list) * id
  | BreakStmt of loc * id
  | ContinueStmt of loc * id

type func =
  { id: id
  ; name: string
  ; num_params: int
  ; num_locals: int
  ; body: (statement list) option
  ; loc: loc
  }

type program = func array array

