open Printf
open Print
open MJ

let indentation = 2


module SM = StringMap

(** [constant2c out c] transpiles the constant [c] to C on the output channel [out]. *)
let constant2c
      out
      (c : MJ.constant)
    : unit =

  match c with
  | ConstBool true  -> fprintf out "1"
  | ConstBool false -> fprintf out "0"
  | ConstInt i      -> fprintf out "%ld" i

(** [binop2c out op] transpiles the binary operator [op] to C on the output channel [out]. *)
let binop2c
      out
      (op : MJ.binop)
    : unit =
  match op with
  | OpAdd -> fprintf out "+"
  | OpSub -> fprintf out "-"
  | OpMul -> fprintf out "*"
  | OpLt  -> fprintf out "<"
  | OpAnd -> fprintf out "&&"

(** [type2c out typ] transpiles the type [typ] to C on the output channel [out]. *)
let type2c
      out
      (typ : MJ.typ)
    : unit =
  let rec type2c out typ = 
    match typ with
    | TypInt -> fprintf out "int"
    | TypBool -> fprintf out "int"
    | TypArray (t,i) -> fprintf out "%a[%ld]" type2c t i
  in 
  type2c out typ

(** [cast out typ] transpiles the cast to [typ] to C on the output channel [out]. *)
let cast
      out
      (typ : MJ.typ)
    : unit =
  fprintf out "(%a)" type2c typ

(** [var2c f out v] transpiles the variable [v] in the context of function [f]
    to C on the output channel [out]. *)
let var2c
      (function_name : string)
      out
      (v : string)
    : unit =
  fprintf out "%s" v

(** [expr2c f out e] transpiles the expression [e], in the context of function [f],
    to C on the output channel [out]. *)
let expr2c
      (function_name : string)
      out
      (expr : MJ.expression)
    : unit =
  let rec expr2c out e =
    match e with
    | EConst const ->
       fprintf out "%a" constant2c const

    | EGetVar v ->
       var2c function_name out v
       
    | EFunctionCall (callee, args) ->
      fprintf out "%s(%a)"
      callee
      (sep_list comma expr2c) args

    | EArrayGet (ea, ei) ->
      fprintf out "%a[%a]"
      expr2c ea
      expr2c ei

    | EUnOp (UOpNot, e) ->
       fprintf out "!(%a)"
         expr2c e

    | EBinOp (op, e1, e2) ->
       fprintf out "(%a %a %a)"
         expr2c e1
         binop2c op
         expr2c e2
  in
  expr2c out expr

(** [instr2c f out ins] transpiles the instruction [ins], in the context of function [f],
    to C on the output channel [out]. *)
let instr2c
      (function_name : string)
      out
      (ins : MJ.instruction)
    : unit =
  let rec instr2c out ins =
    match ins with
    | ISetVar (x, e) ->
       fprintf out "%a = %a;"
         (var2c function_name) x
         (expr2c function_name) e

    | IArraySet (id, ei, ev) ->
      failwith "IArraySet : not implemented yet"

    | IIf (c, i1, i2) ->
       fprintf out "if (%a) %a%telse %a"
         (expr2c function_name) c
         instr2c i1
         nl
         instr2c i2

    | IWhile (c, i) ->
       fprintf out "while (%a) %a"
         (expr2c function_name) c
         instr2c i

    | IBlock is ->
       fprintf out "{%a%t}"
         (indent indentation (sep_list nl instr2c)) is
         nl

    | ISyso e ->
       fprintf out "printf(\"%%d\\n\", %a);"
         (expr2c function_name) e
  in
  instr2c out ins

(** [decl2c out (id, t)] transpiles the declaration [(id, t)] to C on the output channel [out]. *)
let decl2c
      out
      ((id, t) : string * MJ.typ)
    : unit =
  fprintf out "%a %s"
    type2c t
    id

(** [function_definition2c out (name, c)] transpiles all the definitions of the function [name]
    to C on the output channel [out]. *)
let function_definition2c out ((function_name, f) : string * MJ.functio) =
  let return2c out e =
    fprintf out "return (%a);"
      (expr2c function_name) e
  in
  fprintf out "%a %s(%a) {%a%a%a\n}"
    type2c f.result
    function_name
    (sep_list comma decl2c) f.formals
    (term_list semicolon (indent indentation decl2c))
    f.locals
    (list (indent indentation (instr2c function_name))) f.body
    (indent indentation return2c) f.return


(** [all_variables p] returns the list of all the variables of program [p]. *)
let all_variables (p : MJ.program) : string list =
  let variables_from_function (f : MJ.functio) : string list =
    List.(map fst f.formals
          @ map fst f.locals)
  in
    List.(map
            (fun (_, functio) ->
              variables_from_function functio)
            p.defs
          |> flatten)

let program2c out (p : MJ.program) : unit =
  fprintf out
    "#include <stdio.h>\n\
     #include <stdlib.h>\n\
     #pragma GCC diagnostic ignored \"-Wpointer-to-int-cast\"\n\
     #pragma GCC diagnostic ignored \"-Wint-to-pointer-cast\"\n\n\
     %a\
     int main(int argc, char *argv[]) {\
     %a\n\
     %a\n\
     }\n"
    (term_list nl function_definition2c)
    p.defs

    (* MAIN  *)

    (indent indentation (instr2c "main"))
    p.main

    (indent indentation print_string) "return 0;"
