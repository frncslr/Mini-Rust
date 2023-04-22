open LMJ
open Printf

module SM = StringMap
module S = StringSet

type function_type = typ list * typ (** Parameters types and return type of a function. *)

type function_env = function_type SM.t

type variable_env = typ SM.t

exception Error of string

(** [error loc msg] raises an exception [Error] with the message [msg] and the
    position informations associated with [loc]. *)
let error (location : 'a Location.t) (msg : string) =
  raise (Error (sprintf "%s:\n%s"
                  (Error.positions (Location.startpos location) (Location.endpos location))
                  msg))

(** [error locs msg] raises an exception [Error] with the message [msg] and all
    the position informations of the list [locs]. *)
let errors (locations : 'a Location.t list) (msg : string) =
  raise (Error (sprintf "%s%s"
                  (List.fold_right (fun location acc ->
                      sprintf "%s:\n%s" (Error.positions (Location.startpos location) (Location.endpos location)) acc
                   ) locations "") msg))

(** [lookup msg id env] lookups the identifier [id] in the map [env].
    If the identifier is not present raises an error using the message [msg]. *)
let lookup (msg : string) (id : identifier) (env : 'a SM.t) =
  try
    SM.find (Location.content id) env
  with Not_found ->
    error id (sprintf "%s %s is undefined" msg (Location.content id))

(** [vlookup id env] lookups the variable [id] in the environment for variables (locals or parameters) [env]. *)
let vlookup : identifier -> variable_env -> typ = lookup "variable"

(** [mlookup f env] lookups the function [f] in the environment for functions [env]. *)
let mlookup : identifier -> function_env -> function_type = lookup "function"

(** [compatible t1 t2] returns true iff the type [t1] is compatible with type [t2]. *)
let rec compatible (typ1 : typ) (typ2 : typ) : bool =
  match typ1, typ2 with
  | TypInt, TypInt
  | TypBool, TypBool -> true
  | TypArray (t1,i1), TypArray (t2,i2) -> compatible t1 t2 && i1=i2
  | _, _ -> false

(** [type_to_string t] converts the type [t] into a string representation. *)
let rec type_to_string : typ -> string = function
  | TypInt -> "i32"
  | TypBool -> "boolean"
  | TypArray (t,i) -> Printf.sprintf "[%s,%ld]" (type_to_string t) i

(** [typecheck_call fenv venv vinit o callee es] checks, using the environments [fenv] and [venv],
    the set of initialized variables [vinit], that
     * the expression [o] is an object of type [t],
     * the parameters [es] are compatibles with the types of the formal parameters.
    If [typecheck_call] succeeds, the return type of [callee] is returned. *)
let rec typecheck_functioncall (fenv : function_env) (venv : variable_env) (vinit : S.t)
    (callee : identifier)
    (expressions : expression list) : typ =
  let (formals : typ list), (result : typ) = mlookup callee fenv in
  try
    List.iter2 (typecheck_expression_expecting fenv venv vinit) formals expressions;
    result
  with Invalid_argument _ ->
    error callee
      (sprintf "Invalid function call, expected %d arguments, got %d"
        (List.length formals)
        (List.length expressions))


(** [typecheck_expression_expecting fenv venv vinit typ1 e] checks, using the
    environments [fenv] and [venv], the set of initialized variables [vinit],
    that the expression [e] has a type compatible with type [typ1]. *)
and typecheck_expression_expecting (fenv : function_env) (venv : variable_env) (vinit : S.t)
    (typ1 : typ)
    (e : expression) : unit =
  let typ2 = typecheck_expression fenv venv vinit e in
  if not (compatible typ2 typ1) then
    error e
      (sprintf "Type mismatch, expected %s, got %s" (type_to_string typ1) (type_to_string typ2))

(** [typecheck_expression fenv venv vinit e] checks, using the environments [fenv] and
    [venv], the set of initialized variables [vinit], that the expression [e] is well typed.
    If [typecheck_expression] succeeds, the type of [e] is returned. *)
and typecheck_expression (fenv : function_env) (venv : variable_env) (vinit : S.t)
    (e : expression) : typ =
  match Location.content e with
  | EConst (ConstBool _) -> TypBool

  | EConst (ConstInt _) -> TypInt

  | EGetVar v ->
     let typ = vlookup v venv in
     let v' = Location.content v in
     if not (S.mem v' vinit) then
       error v (sprintf "Variable %s has not been initialized" v');
     typ

  | EUnOp (op, e) ->
      let expected, returned =
        match op with
        | UOpNot -> TypBool, TypBool
      in
      typecheck_expression_expecting fenv venv vinit expected e;
      returned

  | EBinOp (op, e1, e2) ->
      let expected, returned =
        match op with
        | OpAdd
        | OpSub
        | OpMul -> TypInt, TypInt
        | OpLt  -> TypInt, TypBool
        | OpAnd -> TypBool, TypBool
      in
      typecheck_expression_expecting fenv venv vinit expected e1;
      typecheck_expression_expecting fenv venv vinit expected e2;
      returned

  | EFunctionCall (callee, expressions) ->
    typecheck_functioncall fenv venv vinit callee expressions

  | EArrayGet (earray, eindex) ->
    typecheck_expression_expecting fenv venv vinit TypInt eindex;
    let expected_type = typecheck_expression fenv venv vinit earray in
    match expected_type with
    | TypArray (t,_) -> t
    | _ as t -> error e (sprintf "Type mismatch, expected TypArray(_), got %s" (type_to_string t))

(** [typecheck_instruction fenv venv vinit inst] checks, using the environments [fenv] and
    [venv], the set of initialized variables [vinit], that the instruction [inst] is well typed.
    If [typecheck_instruction] succeeds, the new set of initialized variables is returned. *)
let rec typecheck_instruction (fenv : function_env) (venv : variable_env) (vinit : S.t)
    (inst : instruction) : S.t =
  match inst with
  | ISetVar (v, e) ->
     let vinit =
       S.add (Location.content v) vinit
     in
     typecheck_expression_expecting fenv venv vinit (vlookup v venv) e;
     vinit

  | IArraySet (earray, eindex, evalue) ->
    typecheck_expression_expecting fenv venv vinit TypInt eindex;
    let array_type = typecheck_expression fenv venv vinit
      (Location.make (Location.startpos earray) (Location.endpos earray) (EGetVar earray)) in
    let value_type = typecheck_expression fenv venv vinit evalue in
    begin
    match array_type,value_type with
    | TypArray (t1,_), t2 -> if not (compatible t1 t2) then error earray (sprintf "Type mismatch, expected TypArray(_), got %s" (type_to_string t1))
    | _ as t, _ -> error earray (sprintf "Type mismatch, expected TypArray(_), got %s" (type_to_string t))
    end;
    vinit

  | IBlock instructions ->
     List.fold_left
       (fun vinit inst ->
         typecheck_instruction fenv venv vinit inst)
       vinit
       instructions

  | IIf (cond, ithen, ielse) ->
    typecheck_expression_expecting fenv venv vinit TypBool cond;
    let vinit1 =
      typecheck_instruction fenv venv vinit ithen
    in
    let vinit2 =
      typecheck_instruction fenv venv vinit ielse
    in
    S.inter vinit1 vinit2

  | IWhile (cond, ibody) ->
    typecheck_expression_expecting fenv venv vinit TypBool cond;
    typecheck_instruction fenv venv vinit ibody

  | ISyso e ->
    (** No typechecking needed as we print '%p', ie a 'void*' *)
     vinit

(** [occurences x bindings] returns the elements in [bindings] that have [x] has identifier. *)
let occurrences (x : string) (bindings : (identifier * 'a) list) : identifier list =
  List.map fst (List.filter (fun (id, _) -> x = Location.content id) bindings)

(** [map_of_association_list entity bindings] creates a map from the association list [bindings].
    If some identifiers are duplicated, [map_of_association_list] raises an [Error] exception,
    using the string [entity] in the error message. *)
let map_of_association_list (entity : string) (bindings : (identifier * 'a) list) : 'a SM.t =
  try
    SM.of_association_list (List.map (fun (id, data) -> (Location.content id, data)) bindings)
  with SM.Duplicate x ->
    errors (occurrences x bindings) (sprintf "%s %s is declared more than once" entity x)

(** [variable_map decls] creates an environment for variables using the association list [decls]. *)
let variable_map (decls : (identifier * typ) list) : variable_env =
  map_of_association_list "Variable" decls

(** [function_map decls] creates an environment for functions using the association list [decls]. *)
let function_map (decls : (identifier * function_type) list) : function_env =
  map_of_association_list "function" decls

(** [typecheck_function fenv venv f] checks, using the environments [fenv] and [venv], 
    that the function [f] is well typed. *)
let typecheck_function (fenv : function_env) (venv : variable_env)
    (f : functio) : unit =

  let formals = f.formals
  and locals = f.locals in

  let mformals = variable_map formals
  and mlocals = variable_map locals in

  begin
    try
      let x =
        StringSet.choose
          (StringSet.inter
             (SM.domain mformals)
             (SM.domain mlocals))
      in
      errors (occurrences x formals @ occurrences x locals)
        "A formal parameter and a local variable cannot carry the same name"
    with Not_found ->
      ()
  end;

  let venv =
    SM.addm mformals venv
  |> SM.addm mlocals
  in

  let vinit =
    S.diff (SM.domain venv) (SM.domain mlocals)
  in
  let vinit =
    typecheck_instruction fenv venv vinit (IBlock f.body)
  in
  typecheck_expression_expecting fenv venv vinit f.result f.return

(** [extract_function_type f] creates a [function_type] from the function [f]. *)
let extract_function_type (f : functio) : function_type =
  (List.map snd f.formals, f.result)

  
let typecheck_program (p : program) : unit =
  let function_id = List.map fst p.defs in 
  let function_t = List.map extract_function_type (List.map snd p.defs) in 
  let fenv = function_map (List.combine function_id function_t) in
  let venv = SM.empty in
  List.iter (typecheck_function fenv venv) (List.map snd p.defs);
  typecheck_instruction fenv venv S.empty p.main
  |> ignore
