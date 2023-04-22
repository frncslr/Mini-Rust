(** We get rid of position informations. *)

let rec translate_expression e = translate_raw_expression (Location.content e)

and translate_raw_expression = function
| LMJ.EConst c -> MJ.EConst c
| LMJ.EGetVar id -> MJ.EGetVar (Location.content id)
| LMJ.EUnOp (op, e) -> MJ.EUnOp (op, translate_expression e)
| LMJ.EBinOp (op, e1, e2) -> MJ.EBinOp (op, translate_expression e1, translate_expression e2)
| LMJ.EFunctionCall (id, args) -> MJ.EFunctionCall (Location.content id, List.map translate_expression args)
| LMJ.EArrayGet (a, i) -> MJ.EArrayGet (translate_expression a, translate_expression i)

and translate_instruction = function
| LMJ.IBlock is -> MJ.IBlock (List.map translate_instruction is)
| LMJ.IIf (c, i1, i2) -> MJ.IIf (translate_expression c, translate_instruction i1, translate_instruction i2)
| LMJ.IWhile (c, i) -> MJ.IWhile (translate_expression c, translate_instruction i)
| LMJ.ISyso e -> MJ.ISyso (translate_expression e)
| LMJ.ISetVar (id, e) -> MJ.ISetVar (Location.content id, translate_expression e)
| LMJ.IArraySet (a, e1, e2) -> MJ.IArraySet (Location.content a, translate_expression e1, translate_expression e2)

let rec translate_typ = function
| LMJ.TypInt -> MJ.TypInt
| LMJ.TypBool -> MJ.TypBool
| LMJ.TypArray (t, i) -> MJ.TypArray (translate_typ t, i)

let translate_binding f (id, t) = (Location.content id, f t)

let translate_bindings f bindings =
  List.map (translate_binding f) bindings

let translate_metho m =
  {
    MJ.formals = List.map (fun (id, t) -> (Location.content id, translate_typ t)) m.LMJ.formals;
    MJ.result  = translate_typ m.LMJ.result;
    MJ.locals  = translate_bindings translate_typ m.LMJ.locals;
    MJ.body    = List.map translate_instruction m.LMJ.body;
    MJ.return  = translate_expression m.LMJ.return
  }

let translate_program p =
  {
    MJ.defs = translate_bindings translate_metho p.LMJ.defs;
    MJ.main = translate_instruction p.LMJ.main;
  }
