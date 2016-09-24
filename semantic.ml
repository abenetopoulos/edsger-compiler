(*TODO: 
   - check that labels in SBreak/SReturn/SContinue exist. Do stuff with labels in SFor
   - during binary assignment, we currently allow the following:
        int *pint;
        int *anotherPint;
        anotherPint = &pint;   
     this should not be allowed without a cast

*)

open Ast
open Types
open Symbol
open Identifier
open Error
open List

exception Terminate of string

type value_persistence = LVal | RVal

let get_param_string params = 
    let aux acc p =
        let pStr = 
            (match p with
            | Param (OType(bType, pointerCnt), _) -> (ast_type_string bType) ^ (string_of_int pointerCnt)
            | ParamByRef (OType(bType, pointerCnt), _) -> "r" ^ (ast_type_string bType) ^ (string_of_int pointerCnt)
            ) in
        acc ^ pStr
    in
    List.fold_left aux "" params

let rec print_type someType =
    match someType with
    | TYPE_int -> Printf.printf "int\n"
    | TYPE_char -> Printf.printf "char\n"
    | TYPE_double -> Printf.printf "double\n"
    | TYPE_bool -> Printf.printf "bool\n"
    | TYPE_none -> Printf.printf "none\n"
    | TYPE_pointer (basicType, pointerCnt) -> Printf.printf "%d pointer to " pointerCnt; print_type basicType
    | _ -> ()

let rec type_matcher bType pointerCnt arrOption = 
    let basic = (match bType with
    | TVoid -> TYPE_void
    | TInt -> TYPE_int
    | TBool -> TYPE_bool
    | TChar -> TYPE_char
    | TDouble -> TYPE_double
    )
    in
    let extraPntr = 
    (match arrOption with
     | None -> 0
     | Some x -> 
        let (type1, _) = check_expr x in
        if not (equalType type1 TYPE_int) then exit 1 else 1
    )
    in
    (match pointerCnt with
     | 0 -> if (extraPntr = 0) then basic else TYPE_pointer(basic, 1)
     | _ -> TYPE_pointer (basic, pointerCnt + extraPntr)
    )

and get_act_param_strings exprs = 
    let match_basic_type bType = 
    (match bType with
        | TYPE_int -> ast_type_string TInt
        | TYPE_bool -> ast_type_string TBool
        | TYPE_char -> ast_type_string TChar
        | TYPE_double -> ast_type_string TDouble
        | _ -> raise (Terminate "Not a basic type")
    )
    in
    let generate_tuples expr = 
        let (exprType, exprPersistence) = check_expr expr in
        let bType = 
        (match exprType with
        | TYPE_pointer (bTyp, cnt) -> ( match_basic_type bTyp ) ^ (string_of_int cnt)
        | _ -> match_basic_type exprType ^ "0"
        ) in
        match exprPersistence with
        | Some LVal -> ("r" ^ bType, Some bType)
        | Some RVal -> (bType, None)
        | _ -> raise (Terminate "oops")
    in
    let rec aux tList acc = 
        (match tList with
        | [] -> acc
        | h :: t -> 
            let (value1, value2Option) = h in
            let acc1 = List.map (fun x -> x ^ value1) acc in
            let acc2 = (match value2Option with
                        | None -> acc1
                        | Some value2 -> List.fold_left (fun a x -> (x ^ value2) :: a) acc1 acc
            )
            in aux t acc2
        )
    in
    let tupleList = List.map generate_tuples exprs in
    aux tupleList [""]

and param_entry f p =
    match p with
    | Param (OType(bType, pointerCnt), name) -> newParameter (id_make name) (type_matcher bType pointerCnt None) PASS_BY_VALUE f false
    | ParamByRef (OType(bType, pointerCnt), name) -> newParameter (id_make name) (type_matcher bType pointerCnt None) PASS_BY_REFERENCE f false

and check_ast ast = 
    match ast with
    | [] -> raise (Terminate "AST is empty")
    | _ as tree -> check_program tree

and check_program decls = 
    let _ = List.iter (fun d -> check_declaration d) decls in
    ()

and check_declaration d =
    match d with
    | VarDecl (OType(bType, pointerCnt), declList) ->
        let checkAndEnter var = 
            match var with
            | ADeclarator (id, arrOption) ->
                    try
                        let hashType = type_matcher bType pointerCnt arrOption in
                        let _ = newVariable (id_make id) hashType true in
                        ()
                    with Failure_NewEntry _ -> raise (Terminate "redeclaration of variable not allowed") (*if redeclaration, abort*)
        in List.iter checkAndEnter declList
    | FunDecl (OType(bType, pointerCnt), name, paramOption) ->
        let paramString = match paramOption with
        | None -> "N"
        | Some params -> get_param_string params
        in
        let id = "_" ^ name ^ "_" ^ paramString in
        let ef, found = newFunction (id_make id) false false in
        (match ef.entry_info with
        | ENTRY_function e ->
            if (e.function_pstatus = PARDEF_DEFINE || e.function_pstatus = PARDEF_COMPLETE) then
                if found then 
                begin
                    if not (equalType e.function_result (type_matcher bType pointerCnt None)) then
                        raise (Terminate "Functions that differ only in their return type can't be overloaded")
                end
                else 
                begin
                    forwardFunction ef;
                    (
                    match paramOption with
                    | None -> ()
                    | Some x -> List.iter (fun y -> let _ = param_entry ef y in ()) x 
                    );
                    e.function_result <- type_matcher bType pointerCnt None;
                    ()
                end
        | _ -> exit 1
        )
    | FunDef(OType(bType, pointerCnt), name, paramOption, decls, stmts) ->
        let paramString = match paramOption with
        | None -> "N"
        | Some params -> get_param_string params
        in
        let id = "_" ^ name ^ "_" ^ paramString in
        let ef, found = newFunction (id_make id) true false in 
        (match ef.entry_info with
        | ENTRY_function e ->
            begin
            if not (e.function_pstatus = PARDEF_DEFINE) then
                if not (equalType e.function_result (type_matcher bType pointerCnt None)) then
                    raise (Terminate "Functions that differ only in their return type can't be overloaded")
                else if (e.function_pstatus = PARDEF_COMPLETE) then
                    raise (Terminate "Function redefinition");

            if (e.function_result = TYPE_none) then
                e.function_result <- type_matcher bType pointerCnt None;

            openScope ();

            (match paramOption with
            | None -> ()
            | Some x -> List.iter (fun y -> let _ = param_entry ef y in ()) x
            );

            endFunctionHeader ef (type_matcher bType pointerCnt None);
            List.iter check_declaration decls;
            List.iter (fun stmt -> check_stmt stmt false (type_matcher bType pointerCnt None)) stmts;

            closeScope ();
            
            if not (equalType e.function_result TYPE_void) then
                begin
                    let lastStmt = List.hd (List.rev stmts) in
                    (
                    match lastStmt with
                    | SReturn _ -> ()
                    | _ -> warning "Νo return statement in non - void function"
                    ) 
                end;


            end
        | _ -> exit 1
        )

and check_stmt stmt inLoop retType = 
    match stmt with
    | SSemicolon -> ()
    | SExpr expr -> let _ = check_expr expr in ()
    | SBlock stmts -> List.iter (fun stmt -> check_stmt stmt false retType) stmts
    | SIf (expr, stmt) -> 
            let (exprType, _) = check_expr expr in
            if (equalType exprType TYPE_bool) then 
                check_stmt stmt false retType
            else raise (Terminate "Condition must be of type bool")
    | SIfElse (expr, stmt1, stmt2) ->
            let (exprType, _) = check_expr expr in
            if (equalType exprType TYPE_bool) then 
            begin
                check_stmt stmt1 false retType;
                check_stmt stmt2 false retType
            end
            else raise (Terminate "Condition must be of type bool")
    | SFor (labelOption, initialization, condition, afterthought, stmt) ->
            let _ = (match initialization with
            | None -> ()
            | Some exp -> let _ = check_expr exp in ()
            ) in ();
            let _ = (match condition with
            | None -> ()
            | Some exp -> 
                    let (condType, _) = check_expr exp 
                    in
                        if not (equalType condType TYPE_bool) then
                            raise (Terminate "Condition must be of type bool")
            ) in ();
            let _ = (match afterthought with
            | None -> ()
            | Some exp -> let _ = check_expr exp in ()
            ) in ();
            check_stmt stmt false retType
    | SContinue _ ->
            if (inLoop = false) then
                raise (Terminate "'continue' statement must be inside loop")
            else
                ()
    | SBreak _ ->
            if (inLoop = false) then
                raise (Terminate "'break' statement must be inside loop")
            else
                ()
    | SReturn exprOption ->
            match exprOption with
            | None ->
                if not (equalType retType TYPE_void) then
                    raise (Terminate "Non-void function should return a value")
            | Some expr ->
                let (exprType, _) = check_expr expr in
                if not (equalType exprType retType) then
                    raise (Terminate "Type of return expression must be the same as function return type")
                else
                    ()

and check_expr expr =
    match expr with
    | EId id -> 
            (
            try
                let e = lookupEntry (id_make id) LOOKUP_ALL_SCOPES false in
                (match e.entry_info with
                | ENTRY_variable varInfo -> (varInfo.variable_type, Some LVal)
                | ENTRY_function funInfo -> (funInfo.function_result, Some RVal)
                | ENTRY_parameter paramInfo -> (paramInfo.parameter_type, Some LVal)
                | _ -> (TYPE_none, None)
                )
            with Not_found ->
                raise (Terminate "Undeclared identifier")
            )
    | EExpr expr -> check_expr expr
    | EBool b -> (TYPE_bool, Some RVal)
    | EInt i -> (TYPE_int, Some RVal)
    | EChar c -> (TYPE_char, Some RVal)
    | EDouble d -> (TYPE_double, Some RVal)
    | EString s -> (TYPE_pointer(TYPE_char, 1), Some RVal)
    | ENull -> (TYPE_none, None)
    | EFCall (id, actParams) ->
            let paramStrings = match actParams with
            | None -> ["N"]
            | Some params -> get_act_param_strings params
            in
            let ids = List.map (fun x -> "_" ^ id ^ "_" ^ x) paramStrings in
            let validEntries = List.fold_left (fun a x -> 
                                                try 
                                                    let ef = lookupEntry (id_make x) LOOKUP_ALL_SCOPES false in
                                                    let e = (
                                                        match ef.entry_info with
                                                        | ENTRY_function entr -> entr
                                                        | _ -> raise (Terminate "This should not have happened")
                                                    )in
                                                    (e.function_result, Some RVal) :: a
                                                with Not_found -> a) [] ids 
            in
            let length = List.length validEntries in
            if (length = 0) then
                (
                List.iter (fun x -> Printf.printf "%s\n" x) ids;
                raise (Terminate "No function definition matches this function call") 
                )
            else if (length > 1) then
               raise (Terminate "Ambiguous call to function. Can't resolve") 
            else
                List.hd validEntries
    | EArray (expr, ArrExp arrExpr) ->
            let (typeExpr, _) = check_expr expr in
            (match typeExpr with
            | TYPE_pointer (basicType, pointerCnt) -> 
                let (typeArr, _) = check_expr arrExpr in
                if not (equalType typeArr TYPE_int) then
                    raise (Terminate "Array index must be of type int")
                else
                    let resultType = if (pointerCnt - 1 = 0) then basicType else TYPE_pointer (basicType, pointerCnt - 1)
                    in
                    (resultType, Some LVal)
            | _ -> raise (Terminate "Expression not an array")
            )
    | EUnAssign (unAssOp, opLocation, expr) ->
            let (typeExpr, persistenceExpr) = check_expr expr in
            if (persistenceExpr = Some RVal) then raise (Terminate "Expression is not assignable")
            else
                (typeExpr, Some RVal) (*NOTE(achilles) : in the style of C *)
    | EBinAssign (binAssOp, expr1, expr2) ->
            let (type1, persistence1) = check_expr expr1 in
            if (persistence1 = Some RVal || type1 = TYPE_none) then raise (Terminate "Expression is not assignable")
            else
                let (type2, persistence2) = check_expr expr2 in
                (
                match type1 with
                | TYPE_pointer (basicType, _) ->
                    if (binAssOp = BinAssignPlus || binAssOp = BinAssignMinus) then
                        if (equalType type2 TYPE_int) then (type1, Some RVal)
                        else raise (Terminate "Invalid operands to assignment expression")
                    else if (binAssOp = BinAssign) then
                        (
                        match type2 with
                        | TYPE_pointer (rightBasicType, _) ->
                                if (equalType basicType rightBasicType) then
                                    (type1, Some RVal)
                                else
                                    raise (Terminate "Assigning to incompatible pointer type")
                        | TYPE_none -> (TYPE_none, None)
                        | _ -> raise (Terminate "Invalid operands to assignment expression")
                        )
                    else raise (Terminate "Invalid operands to assignment expression")
                | _ -> 
                    if not (equalType type1 type2) then begin 
                        raise (Terminate "Expressions on either side of operator must be of same type")
                    end
                    else
                        (type1, Some RVal)
                )
    | ECast (OType (bType, pointerCnt) as objType, expr) ->
            let (exprType, _) = check_expr expr in
            if (valid_conversion objType exprType) then
                let newType = type_matcher bType pointerCnt None in
                (newType, Some RVal)
            else
                raise (Terminate "Invalid conversion")
    | EConditional (exprCondition, exprTrue, exprFalse) ->
            let (typeCondition, _) = check_expr exprCondition in
            if not (equalType typeCondition TYPE_bool) then
                raise (Terminate "Condition expression must be of type bool")
            else begin
                let (typeTrue, _) = check_expr exprTrue in
                let (typeFalse, _) = check_expr exprFalse in
                if not (equalType typeTrue typeFalse) then
                    raise (Terminate "conditional branches must be of same type")
                else
                    (typeTrue, Some RVal)
            end
    | ENew (OType(basicType, pointerCnt), arrayOption) ->
            let exprOption = 
                (
                match arrayOption with
                | None -> None
                | Some (ArrExp expr) -> Some expr 
            ) in
            let hashType = type_matcher basicType pointerCnt exprOption in
            (hashType, Some RVal)
    | ENewP (OType(bType, pointerCnt), newExprOption) ->
            let hashType =
                (
                match newExprOption with
                | Some (Some (ArrExp expr), None) -> type_matcher bType pointerCnt (Some expr)
                | Some (None, Some _) -> raise (Terminate "Invalid operands to binary operation")
                | None -> type_matcher bType pointerCnt None
                | _ -> raise (Terminate "oops")
                )
            in
            (hashType, Some RVal)
    | EDelete expr -> (*TODO(achilles): check that memory pointed to by expr was allocated via new*)
            let (exprType, _) = check_expr expr in
            (
            match exprType with
            | TYPE_pointer _ -> (TYPE_none, Some RVal)
            | _ -> raise (Terminate "Can't delete non pointer")
            )
    | EUnary (unOp, expr) ->
            let (exprType, exprPersistence) = check_expr expr in
            (
            match unOp with
            | UnaryRef -> (*if type of expr is pointer, increase cnt by one, else make pointer*)
                    ( (*FIX (achilles) : if expr is a constant, signify error*)
                    match exprType with
                    | TYPE_pointer (basicType, pointerCnt) -> (TYPE_pointer (basicType, pointerCnt + 1), Some RVal)
                    | _ as basicType -> (TYPE_pointer (basicType, 1), Some RVal)
                    )
            | UnaryDeref ->
                    (
                    match exprType with
                    | TYPE_pointer (basicType, pointerCnt) -> 
                            if (pointerCnt > 1) then 
                                (TYPE_pointer (basicType, pointerCnt - 1), Some LVal)
                            else
                                (basicType, Some LVal)
                    | _ -> raise (Terminate "Can't dereference non-pointer")
                    )
            | UnaryPlus | UnaryMinus -> 
                    if ((equalType exprType TYPE_int) || (equalType exprType TYPE_double)) then
                        (exprType, Some RVal)
                    else
                        raise (Terminate "Operand of unary operator should be of type int or double")
            | UnaryNot -> 
                    (TYPE_bool, Some RVal)
            )
    | EBinOp (binOp, expr1, expr2) -> 
            let (type1, _) = check_expr expr1 in
            let (type2, exprPersistence) = check_expr expr2 in
            (
            match binOp with
            | BinComma -> (op_res_type BinComma type2, exprPersistence)
            | BinPlus | BinMinus ->
                    (
                    match type1 with
                    | TYPE_pointer _ ->
                        if (equalType type2 TYPE_int) then (type1, Some RVal)
                        else raise (Terminate "Invalid operands to binary operator")
                    | _ -> 
                            (
                            match type2 with
                            | TYPE_pointer _ -> 
                                if (equalType type1 TYPE_int) then (type2, Some RVal)
                                else raise (Terminate "Invalid operands to binary operator")
                            | _ -> 
                                if (equalType type1 type2) then (type1 , Some RVal)
                                else raise (Terminate "Operands of binary operation must be of same type")
                            )
                    )
            | _ ->
                if (equalType type1 type2) then (op_res_type binOp type1, Some RVal)
                else raise (Terminate "Operands of binary operation must be of same type")
            )

and op_res_type optor opand = 
    match optor with
    | BinDiv | BinMulti | BinPlus | BinMinus | BinComma -> opand
    | BinMod -> TYPE_int
    | BinLess | BinGreater | BinLessEq | BinGreaterEq | BinEq | BinNotEq | BinAnd | BinOr -> TYPE_bool

and valid_conversion resType sourceType = 
    let OType(basicType, pointerCnt) = resType in
    let hashType = type_matcher basicType pointerCnt None in
    match hashType, sourceType with
    | TYPE_pointer _, TYPE_pointer _ -> true
    | TYPE_pointer _, _ -> false
    | _, TYPE_pointer _ -> false
    | _, _ -> true

