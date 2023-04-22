(** This is the same abstract syntax tree as in [LMJ.mli] but without position informations.
    After typechecking, we don't need to give feedbacks to the user. *)

type identifier = string

type expression =
  | EConst of constant
  | EGetVar of identifier
  | EUnOp of unop * expression
  | EBinOp of binop * expression * expression
  | EFunctionCall of identifier * expression list
  | EArrayGet of expression * expression

and constant = LMJ.constant =
  | ConstBool of bool
  | ConstInt of int32

and binop = LMJ.binop =
  | OpAdd
  | OpSub
  | OpMul
  | OpLt
  | OpAnd

and unop = LMJ.unop = UOpNot

and instruction =
  | IBlock of instruction list
  | IIf of expression * instruction * instruction
  | IWhile of expression * instruction
  | ISyso of expression
  | ISetVar of identifier * expression
  | IArraySet of identifier * expression * expression

and typ =
  | TypInt
  | TypBool
  | TypArray of typ * int32

and functio = {
    formals: (identifier * typ) list;
    result: typ;
    locals: (identifier * typ) list;
    body: instruction list;
    return: expression
  }

and program = {
    defs: (identifier * functio) list;
    main: instruction
  }
