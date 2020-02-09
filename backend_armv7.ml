(* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the code generator for ARMv7.
 * The IR is traversed and x86 instructions emulating the
 * stack machine's operation are emitted.
 *)

open Ir

let compile_closure out { id; num_params; num_locals; name; insts; _ } =
  Printf.fprintf out "\t.text %d\n" num_params;
  Printf.fprintf out "_lambda_%d:\n" id;

  (match name with
  | None -> ()
  | Some n ->
    Printf.fprintf out ".global _lambda_%s\n" n;
    Printf.fprintf out "_lambda_%s:\n" n
  );

  Printf.fprintf out "\tstmfd sp!, {r0, fp, lr}\n";
  Printf.fprintf out "\tmov fp, sp\n";
  if num_locals > 0 then
    Printf.fprintf out "\tsub sp, sp, #%d\n" (num_locals * 4);

  Array.iter
    (fun inst -> match inst with
    | GetClosure c ->
      Printf.fprintf out "\tGetClosure %d\n" c;
    | GetBuiltin name ->
      Printf.fprintf out "\tGetBuiltin %s\n" name;
    | GetEnv i ->
      Printf.fprintf out "\tGetEnv %d\n" i;
    | GetArg i ->
      Printf.fprintf out "\tGetArg %d\n" i;
    | GetLocal i ->
      Printf.fprintf out "\tGetLocal %d\n" i;
    | SetLocal i ->
      Printf.fprintf out "\tSetLocal %d\n" i;
    | ConstInt i ->
      Printf.fprintf out "\tConstInt %d\n" i;
    | ConstBool i ->
      let x = (if i then 1 else 0) in
      Printf.fprintf out "\tConstBool %d\n" x;
    | Closure(i, num_capture) ->
      Printf.fprintf out "\tClosure (%d, %d)\n" i num_capture;
    | Add ->
      Printf.fprintf out "\tAdd\n";
    | Subtract ->
      Printf.fprintf out "\tSubtract\n";
    | Equals ->
      Printf.fprintf out "\tEquals\n";
    | Nequals -> 
      Printf.fprintf out "\tNEquals\n";
    | Or2 -> 
      Printf.fprintf out "\tOr\n";
    | And2 -> 
      Printf.fprintf out "\tAnd\n";
    | Label i -> 
      Printf.fprintf out "\tLabel %d\n" i;
    | Jump i -> 
      Printf.fprintf out "\tJump %d\n" i;
    | JumpZ i -> 
      Printf.fprintf out "\tJumpZ %d\n" i;
    | Call ->
      Printf.fprintf out "\tCall\n";
    | Return ->
      Printf.fprintf out "\tReturn\n";
    | Pop ->
      Printf.fprintf out "\tPop\n"
    ) insts;

  Printf.fprintf out "\t.data\n";
  Printf.fprintf out "\t.word 0\n";
  Printf.fprintf out "_lambda_%d_closure:\n" id;
  Printf.fprintf out "\t.word _lambda_%d\n" id

let compile prog out =
  Array.iter (compile_closure out) prog

