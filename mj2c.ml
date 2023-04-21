open Printf
open Print
open MJ


(** [struct_array_name] is the name of the structure that holds an array and its length:
    class array {
          int* array;
          int length;
    };
    But [array] could be the name of a class and in this case, we have to create a new name. *)
let struct_array_name = ref ""

(** [name1] is a fresh name, different from all other variables in the MiniJava program. *)
let name1 = ref ""

(** [name2] is a fresh name, different from all other variables in the MiniJava program,
    and different from [name1]. *)
let name2 = ref ""

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
  match typ with
  | TypInt -> fprintf out "int"
  | TypBool -> fprintf out "bool"
  | TypIntArray -> fprintf out "struct %s*" !struct_array_name
  | Typ t -> fprintf out "struct %s*" t

(** [cast out typ] transpiles the cast to [typ] to C on the output channel [out]. *)
let cast
      out
      (typ : MJ.typ)
    : unit =
  fprintf out "(%a)" type2c typ

(** [var2c m class_info v] transpiles the variable [v] in the context of [class_info] and method [m]
    to C on the output channel [out].
    We must distinguish between an attribute and a local variable or a parameter. *)
let var2c
      (method_name : string)
      out
      (v : string)
    : unit =
  fprintf out "%s" v

(** [expr2c m class_info out e] transpiles the expression [e], in the context of method [m] and [class_info],
    to C on the output channel [out]. *)
let expr2c
      (method_name : string)
      out
      (expr : MJ.expression)
    : unit =
  let rec expr2c out e =
    match e with
    | EConst const ->
       fprintf out "%a" constant2c const

    | EGetVar v ->
       var2c method_name out v

    | ESelf ->
       fprintf out "self"

    | EMethodCall (o, callee, args) ->
      failwith "EMethodCall : not implemented yet"

    | EArrayAlloc e ->
      failwith "EArrayAlloc : not implemented yet"

    | EObjectAlloc id ->
      failwith "EObjectAlloc : not implemented yet"

    | EArrayGet (ea, ei) ->
      failwith "EArrayGet : not implemented yet"

    | EArrayLength e ->
      failwith "EArrayLength : not implemented yet"

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

(** [instr2c m class_info out ins] transpiles the instruction [ins], in the context of method [m] and [class_info],
    to C on the output channel [out]. *)
let instr2c
      (method_name : string)
      out
      (ins : MJ.instruction)
    : unit =
  let rec instr2c out ins =
    match ins with
    | ISetVar (x, e) ->
       fprintf out "%a = %a;"
         (var2c method_name) x
         (expr2c method_name) e

    | IArraySet (id, ei, ev) ->
      failwith "IArraySet : not implemented yet"

    | IIf (c, i1, i2) ->
       fprintf out "if (%a) %a%telse %a"
         (expr2c method_name) c
         instr2c i1
         nl
         instr2c i2

    | IWhile (c, i) ->
       fprintf out "while (%a) %a"
         (expr2c method_name) c
         instr2c i

    | IBlock is ->
       fprintf out "{%a%t}"
         (indent indentation (sep_list nl instr2c)) is
         nl

    | ISyso e ->
       fprintf out "printf(\"%%d\\n\", %a);"
         (expr2c method_name) e
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

(** [method_declaration2c out (name, c)] transpiles all the declarations of the methods of the class [name] with type [c]
    to C on the output channel [out]. *)

let method_declaration2c
      out
      ((method_name, m) : string * MJ.functio)
    : unit =
  fprintf out "void* %s(%a);"
    method_name
    (prec_list comma decl2c)
    m.formals
  

(** [method_definition2c out (name, c)] transpiles all the definitions of the methods of the class [name] with type [c]
    to C on the output channel [out]. *)
let method_definition2c out ((method_name, m) : string * MJ.functio) =
  let return2c out e =
    fprintf out "return (void*)(%a);"
      (expr2c method_name) e
  in
  fprintf out "void* %s(%a) {%a%a%a\n}"
    method_name
    (prec_list comma decl2c) m.formals
    (term_list semicolon (indent indentation decl2c))
    m.locals
    (list (indent indentation (instr2c method_name))) m.body
    (indent indentation return2c) m.return


(** [all_variables p] returns the list of all the variables of program [p]. *)
let all_variables (p : MJ.program) : string list =
  let variables_from_method (m : MJ.functio) : string list =
    List.(map fst m.formals
          @ map fst m.locals)
  in
    List.(map
            (fun (_, functio) ->
              variables_from_method functio)
            p.defs
          |> flatten)

let program2c out (p : MJ.program) : unit =
  let all_func_names =
    List.map fst p.defs
  in
  let rec variant s l =
    if List.mem s l then
      variant (s ^ "_") l
    else
      s
  in
  struct_array_name := variant "array" all_func_names;
  let all_variables = all_variables p in
  name1 := variant "tmp1" all_variables;
  name2 := variant "tmp2" (!name1 :: all_variables);
  fprintf out
    "#include <stdio.h>\n\
     #include <stdlib.h>\n\
     // #include \"tgc.h\"\n\
     #pragma GCC diagnostic ignored \"-Wpointer-to-int-cast\"\n\
     #pragma GCC diagnostic ignored \"-Wint-to-pointer-cast\"\n\
     struct %s { int* array; int length; };\n\
     // tgc_t gc;\n\
     %a\
     %a\
     int main(int argc, char *argv[]) {\
     %a\n\
     %a\n\
     }\n"
    !struct_array_name

    (term_list nl method_declaration2c)
    p.defs

    (term_list nl method_definition2c)
    p.defs

    (* MAIN  *)

    (* (indent indentation print_string) "tgc_start(&gc, &argc);" *)

    (indent indentation (instr2c "main"))
    p.main

    (* (indent indentation print_string) "tgc_stop(&gc);" *)

    (indent indentation print_string) "return 0;"
