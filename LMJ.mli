(** The abstract syntax tree for Mini-Rust with position informations
    to point the user to the location of the error in the source file
    when a syntax or typechecking error is encountered.
 *)

(** An identifier with position informations. *)
type identifier = string Location.t

(** An expression with position informations. *)
type expression = raw_expression Location.t

(** An expression without position informations. *)
and raw_expression =
  | EConst of constant (** A integer or boolean constant. *)
  | EGetVar of identifier (** Get the value of a variable. *)
  | EUnOp of unop * expression (** An unary operator. *)
  | EBinOp of binop * expression * expression (** [EBinOp (op, e1, e2)] represents the expression [e1 op e2]. *)
  | EFunctionCall of identifier * expression list (** [EFunctionCall (id, [p1, ..., pn])] represents the call [id(p1, ..., pn)]. *)
  | EArrayGet of expression * expression (** [EArrayGet (e1, e2)] represents the expression [e1[e2]]. *)

and constant =
  | ConstBool of bool (** Boolean constant [true] or [false]. *)
  | ConstInt of int32 (** Integer constant [[-2^31, 2^31 - 1]]. *)

and binop =
  | OpAdd (** Binary operator [+]. *)
  | OpSub (** Binary operator [-]. *)
  | OpMul (** Binary operator [*]. *)
  | OpLt  (** Binary operator [<]. *)
  | OpAnd (** Binary operator [&&]. *)

and unop = UOpNot (** Unary operator [!]. *)

and instruction =
  | IBlock of instruction list (** [IBlock [i1; i2; ...; in]] represents the instruction [{ i1 i2 ... in }]. *)
  | IIf of expression * instruction * instruction (** [IIf (e, i1, i2)] represents the instruction [if (e) i1 else i2]. *)
  | IWhile of expression * instruction (** [IWile (e, ins)] represents the instruction [while (e) ins]. *)
  | ISyso of expression (** [ISyso e] represents the instruction [System.out.println(e);]. *)
  | ISetVar of identifier * expression (** [ISetVar (id, e)] represents the instruction [id = e;]. *)
  | IArraySet of identifier * expression * expression (** [IArraySet (id, e1, e2)] represents the instruction [id[e1] = e2;]. *)

and typ =
  | TypInt (** Type [int]. *)
  | TypBool (** Type [bool]. *)
  | TypArray of typ * int32 (** Type [typ[]], an array with an int32 size . *)

and functio = {
  formals: (identifier * typ) list; (** The names of the parameters of the function with their types. *)
  result: typ; (** Result type of the function. *)
  locals: (identifier * typ) list; (** The names of the local variables with their types (declared at the beginning of the function). *)
  body: instruction list; (** The list of instructions of the function. *)
  return: expression (** The return expression. *)
}

and program = {
  defs: (identifier * functio) list; (** The names and definitions of the other functions. *)
  main: instruction (** In MiniRust the main has only one INSTRUCTION (but you can use
                        a block if you want more than one). *)
}
