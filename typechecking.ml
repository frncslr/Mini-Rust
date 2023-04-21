open LMJ
open Printf

module SM = StringMap
module S = StringSet

type method_type = typ list * typ (** Parameters types and return type of a method. *)

type method_env = method_type SM.t

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

(** [mlookup m env] lookups the method [m] in the environment for methods [env]. *)
let mlookup : identifier -> method_env -> method_type = lookup "method"

(** [compatible t1 t2 instanceof] returns true iff the type [t1] is compatible with type [t2].
    For classes, uses the function [instanceof] to decide if [t1] is an instance of [t2]. *)
let rec compatible (typ1 : typ) (typ2 : typ) : bool =
  match typ1, typ2 with
  | TypInt, TypInt
  | TypBool, TypBool
  | TypIntArray, TypIntArray -> true
  | _, _ -> false

(** [type_to_string t] converts the type [t] into a string representation. *)
let rec type_to_string : typ -> string = function
  | TypInt -> "i32"
  | TypBool -> "boolean"
  | TypIntArray -> "[i32,i32]"
  | Typ t -> Location.content t

(** [typecheck_call cenv venv vinit instanceof o callee es] checks, using the environments [cenv] and [venv],
    the set of initialized variables [vinit] and the [instanceof] function, that
     * the expression [o] is an object of type [t],
     * the method [callee] belongs to the class [t],
     * the parameters [es] are compatibles with the types of the formal parameters.
    If [typecheck_call] succeeds, the return type of [callee] is returned. *)
let rec typecheck_call (menv : method_env) (venv : variable_env) (vinit : S.t)
    (o : expression)
    (callee : identifier)
    (expressions : expression list) : typ =
  let o_type = typecheck_expression menv venv vinit o in
  match o_type with
  | Typ t ->
    begin
      let (formals : typ list), (result : typ) = mlookup callee menv in
      try
        List.iter2 (typecheck_expression_expecting menv venv vinit) formals expressions;
        result
      with Invalid_argument _ ->
        error callee
          (sprintf "Invalid function call, expected %d arguments, got %d"
             (List.length formals)
             (List.length expressions))
    end
  | _ -> error o (sprintf "A class is expected, got %s" (type_to_string o_type))


(** [typecheck_expression_expecting cenv venv vinit instanceof typ1 e] checks, using the
    environments [cenv] and [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the expression [e] has a type compatible with type [typ1]. *)
and typecheck_expression_expecting (menv : method_env) (venv : variable_env) (vinit : S.t)
    (typ1 : typ)
    (e : expression) : unit =
  let typ2 = typecheck_expression menv venv vinit e in
  if not (compatible typ2 typ1) then
    error e
      (sprintf "Type mismatch, expected %s, got %s" (type_to_string typ1) (type_to_string typ2))

(** [typecheck_expression cenv venv vinit instanceof e] checks, using the environments [cenv] and
    [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the expression [e] is well typed.
    If [typecheck_expression] succeeds, the type of [e] is returned. *)
and typecheck_expression (menv : method_env) (venv : variable_env) (vinit : S.t)
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
      typecheck_expression_expecting menv venv vinit expected e;
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
      typecheck_expression_expecting menv venv vinit expected e1;
      typecheck_expression_expecting menv venv vinit expected e2;
      returned

  | EMethodCall (o, callee, expressions) ->
     typecheck_call menv venv vinit o callee expressions

  | EArrayGet (earray, eindex) ->
    typecheck_expression_expecting menv venv vinit TypInt eindex;
    typecheck_expression_expecting menv venv vinit TypIntArray earray;
    TypInt

  | EArrayAlloc elength ->
    typecheck_expression_expecting menv venv vinit TypInt elength;
    TypIntArray

  | EArrayLength earray ->
    typecheck_expression_expecting menv venv vinit TypIntArray earray;
    TypInt

  (* | ESelf ->
     vlookup (Location.make (Location.startpos e) (Location.endpos e) "this") venv

  | EObjectAlloc id ->
      clookup id cenv |> ignore;
      Typ id *)

(** [typecheck_instruction cenv venv vinit instanceof inst] checks, using the environments [cenv] and
    [venv], the set of initialized variables [vinit] and the [instanceof] function,
    that the instruction [inst] is well typed.
    If [typecheck_instruction] succeeds, the new set of initialized variables is returned. *)
let rec typecheck_instruction (menv : method_env) (venv : variable_env) (vinit : S.t)
    (inst : instruction) : S.t =
  match inst with
  | ISetVar (v, e) ->
     let vinit =
       S.add (Location.content v) vinit
     in
     typecheck_expression_expecting menv venv vinit (vlookup v venv) e;
     vinit

  | IArraySet (earray, eindex, evalue) ->
    typecheck_expression_expecting menv venv vinit TypIntArray
      (Location.make (Location.startpos earray) (Location.endpos earray) (EGetVar earray));
    typecheck_expression_expecting menv venv vinit TypInt eindex;
    typecheck_expression_expecting menv venv vinit TypInt evalue;
    vinit

  | IBlock instructions ->
     List.fold_left
       (fun vinit inst ->
         typecheck_instruction menv venv vinit inst)
       vinit
       instructions

  | IIf (cond, ithen, ielse) ->
    typecheck_expression_expecting menv venv vinit TypBool cond;
    let vinit1 =
      typecheck_instruction menv venv vinit ithen
    in
    let vinit2 =
      typecheck_instruction menv venv vinit ielse
    in
    S.inter vinit1 vinit2

  | IWhile (cond, ibody) ->
    typecheck_expression_expecting menv venv vinit TypBool cond;
    typecheck_instruction menv venv vinit ibody

  | ISyso e ->
     typecheck_expression_expecting menv venv vinit TypInt e;
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

(** [method_map decls] creates an environment for methods using the association list [decls]. *)
let method_map (decls : (identifier * method_type) list) : method_env =
  map_of_association_list "Method" decls

(** [typecheck_method cenv venv instanceof m] checks, using the environments [cenv] and [venv]
    and the [instanceof] function, that the method [m] is well typed. *)
let typecheck_method (menv : method_env) (venv : variable_env)
    (m : functio) : unit =

  let formals = m.formals
  and locals = m.locals in

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
    typecheck_instruction menv venv vinit (IBlock m.body)
  in
  typecheck_expression_expecting menv venv vinit m.result m.return

(** [extract_method_type m] creates a [method_type] from the method [m]. *)
let extract_method_type (m : functio) : method_type =
  (List.map snd m.formals, m.result)

  
let typecheck_program (p : program) : unit =
  let method_id = List.map fst p.defs in 
  let method_t = List.map extract_method_type (List.map snd p.defs) in 
  let menv = method_map (List.combine method_id method_t) in
  let venv = SM.empty in
  List.iter (typecheck_method menv venv) (List.map snd p.defs);
  typecheck_instruction menv venv S.empty p.main
  |> ignore
