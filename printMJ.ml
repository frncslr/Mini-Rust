open Printf
open Print
open MJ

let indentation = 2

(** [constant out c] prints the constant [c] on the output channel [out]. *)
let constant out = function
  | ConstBool true ->
     fprintf out "true"
  | ConstBool false ->
     fprintf out "false"
  | ConstInt i ->
     fprintf out "%ld" i

(** [binop out op] prints the binary operator [op] on the output channel [out]. *)
let binop out = function
  | OpAdd ->
     fprintf out "+"
  | OpSub ->
     fprintf out "-"
  | OpMul ->
     fprintf out "*"
  | OpLt  ->
     fprintf out "<"
  | OpAnd ->
     fprintf out "&&"

(** [expr out e], [expr1 out e], ..., [expr6 out e] print the expression [e]
    on the output channel [out]. [expr] is a synonym for [expr6].
    We have different functions to minimize the number of parenthesis. An expression
    doesn't need parenthesis if the priority of its operands is greater or equal to
    its priority.
    [expr6] handles the expressions of least priority and [expr1] handles the expressions
    of greatest priority. It's in the default case of [expr1] that we put parenthesis, in
    this case, we have an expression of lower priority than the current context and so we
    have to put parenthesis around it and then call [expr] again. *)
let rec expr1 out = function
  | EConst c ->
     fprintf out "%a" constant c
  | EGetVar x ->
     fprintf out "%s" x
  | EFunctionCall (c, es) ->
     fprintf out "%s(%a)"
       c
       (sep_list comma expr) es
  | EArrayGet (ea, ei) ->
     fprintf out "%a[%a]"
       expr ea
       expr ei
  | e ->
     fprintf out "(%a)"
       expr e

and expr2 out = function
  | EUnOp (UOpNot, e) ->
     fprintf out "!%a"
       expr2 e
  | e ->
     expr1 out e

and expr3 out = function
  | EBinOp (OpMul as op, e1, e2) ->
     fprintf out "%a %a %a"
       expr3 e1
       binop op
       expr3 e2
  | e ->
     expr2 out e

and expr4 out = function
  | EBinOp (OpSub as op, e1, e2) ->
     fprintf out "%a %a %a"
       expr4 e1
       binop op
       expr3 e2
  | e ->
     expr3 out e

and expr5 out = function
  | EBinOp (OpAdd as op, e1, e2) ->
     fprintf out "%a %a %a"
       expr5 e1
       binop op
       expr5 e2
  | e ->
     expr4 out e

and expr6 out = function
  | EBinOp ((OpLt | OpAnd) as op, e1, e2) ->
     fprintf out "%a %a %a"
       expr6 e1
       binop op
       expr6 e2
  | e ->
     expr5 out e

and expr out e =
  expr6 out e

(** [binop out ins] prints the instruction [ins] on the output channel [out]. *)
let rec instr out = function
  | ISetVar (x, e) ->
     fprintf out "%s = %a;"
       x
       expr e
  | IArraySet (id, ei, ev) ->
     fprintf out "%s[%a] = %a;"
       id
       expr ei
       expr ev
  | IIf (c, i1, i2) ->
      fprintf out "if (%a) %a%telse %a"
        expr c
        instr i1
        nl
        instr i2
  | IWhile (c, i) ->
      fprintf out "while (%a) %a"
        expr c
        instr i
  | IBlock is ->
     fprintf out "{%a%t}"
       (indent indentation (sep_list nl instr)) is
       nl
  | ISyso e ->
     fprintf out "println!(\"{}\",%a);"
       expr e

(** [typ out t] prints the type [t] on the output channel [out]. *)
let rec typ out = function
  | TypInt ->
     fprintf out "i32"
  | TypBool ->
     fprintf out "bool"
  | TypArray (t,i) ->
     fprintf out "%a[%ld]" typ t i

(** [typ out (x, t)] prints the type [t] and the associated variable [x] on the output channel [out]. *)
let binding out (x, t) =
  fprintf out "let %s : %a "
   x
   typ t

(** [functio out (name, m)] prints the method [name] with type [MJ.functio m] on the output channel [out]. *)
let functio out (name, m) =
  fprintf out "fn %s(%a) -> %a {%a%a%t%t}"
    name
    (sep_list comma binding) m.formals
    typ m.result
    (term_list semicolon (indent indentation binding)) m.locals
    (list (indent indentation instr)) m.body
    (indent_t indentation (fun out -> fprintf out "return %a;" expr m.return))
    nl

(** [clas out (name, c)] prints the clas [name] with type [MJ.clas c] on the output channel [out]. *)
(* let clas out (name, c) =
  (match c.extends with
   | None ->
      fprintf out "class %s {%a%a%t}" name
   | Some class_name ->
      fprintf out "class %s extends %s {%a%a%t}" name class_name)
    (term_list semicolon (indent indentation binding)) c.attributes
    (list (indent indentation functio)) c.methods
    nl *)

let print_program out (p : MJ.program) : unit =
  fprintf out "%t%t %t%a"
    (indent_t indentation
       (fun out ->
         fprintf out "fn main() {%a%t}"
           (indent indentation instr) p.main
           nl))
    nl
    nl
    (sep_list nl functio) p.defs
