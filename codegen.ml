let llctx = global_context ()

let llm = create_module llctx "program_module"

let int_type = i16_type llctx
let char_type = i8_type llctx
let double_type = x86fp80_type llctx
let bool_type = i1_type llctx
let non_type = void_type llctx

type scope_type = SGlobal | SInternal

let code_gen ast =
    match ast with
    | [] -> raise (Terminate "AST is empty")
    | _ as tree ->
            let bldr = builder llctx in
            List.iter (fun x -> generate_code x SGlobal None bldr) tree

let rec generate_code node scope envOpt bldr =
    match node with
    | VarDecl (OType(bType, pointerCnt), declList) ->
        let basellType = get_llvm_type bType pointerCnt in
        let extract_additional x =
            let ADeclarator(xName, xExprOption) = x in
            (
                match xExprOption with
                | None -> (xName, 0)
                | Some expr -> 
                        let nItems = evaluate_number expr in 
                        (xName, nItems)
            )
        in
        let additionalllTypeOptions = List.map extract_additional declList in
        if (scope = SGlobal) then
            List.iter2 (fun bllType (vName, additional) -> 
                match additional with 
                | 0 -> declare_global bllType vName llm
                | _ as nItems -> declare_global (array_type bllType nItems) vName llm
                ) basellType additionalllTypeOptions
        else
            let Some env = envOpt in
            List.iter2 (fun bllType (vName, additional) -> 
                let llval = 
                    match additional with 
                    | 0 -> build_alloca bllType vName bldr
                    | _ as nItems -> build_alloca (array_type bllType nItems) vName bldr
                in
                Hashtbl.add env vName llval
                ) basellType additionalllTypeOptions
    | FunDecl (OType(bType, pointerCnt), name, paramOption) ->
            let llType = get_llvm_type bType pointerCnt in
            let paramArray = make_param_array paramOption in
            let fType = function_type llType paramArray in
            let func = declare_function name fType llm in
            ()
    | FunDef(OType(bType, pointerCnt), name, paramOption, decls, stmts) ->
            let llType = get_llvm_type bType pointerCnt in
            let paramArray = make_param_array paramOption in
            let fType = function_type llType paramArray in
            let func = define_function name ftype llm in
            (*let fBuilder = builder_at_end llctx (entry_block f) in*)
            let _ = position_at_end (entry_block f) bldr in
            let env:(string, llvalue) Hashtbl.t = Hashtbl.create (List.length declValues) in
            let _ = List.iter (fun d -> generate_code d SInternal (Some env) bldr) decls in
            let _ = List.iter (fun d -> codegen_stmt d env bldr) stmts in
            (*let _ = List.iter (fun d -> generate_code d SInternal bldr) decls in
            let _ = List.iter (fun d -> generate_code d SInternal bldr) stmts in*)
            if (llType = non_type) then
                let _ = build_ret_void bldr in
                ()
            else
                ()
        
and codegen_stmt stmt env bldr = 
    match stmt with
    | SExpr expr -> ()
    | SBlock stmts -> List.iter (fun d -> codegen_stmt d env bldr) stmts
    | SIf (condExpr, stmt) ->
            let predicate, lhs, rhs = eval_bool_expr condExpr in
            let cmprson = build_icmp predicate lhs rhs "tmp" in (*check type of sides to build cmp*)
            let start_bb = insertion_block bldr in
            let parent_func = block_parent start_bb in
            let then_bb = append_block llctx "then" parent_func in
            let _ = position_at_end then_bb bldr in
            let _ = generate_code stmt SInternal bldr in
            ()
    | SIfElse (expr, stmt1, stmt2) -> ()
    | SFor (labelOption, initialization, condition, afterthought, stmt) -> ()
    | SContinue labelOption -> ()
    | SBreak labelOption -> ()
    | SReturn exprOption -> ()

and codegen_expr expr env bldr = 
    match expr with
    | EId name -> locate_llval name
    | EExpr nExpr -> codegen_expr nExpr env bldr
    | EBool b -> 
            let x = if b = true then 1 else 0 in 
            const_int bool_type x
    | EInt i -> const_int int_type i
    | EChar c -> 
            let ascii = code c in
            const_int char_type ascii
    | EDouble d -> const_float double_type d (*NOTE: will this automatically convert ocaml's float into extended precision float?*)
    | EString s -> const_stringz llctx s
    | ENull -> const_null int_type           (*NOTE: type is irrelevant to us*)
    | EFCall (fName, exprList) ->
        let Some funLLValue = lookup_function fName llmod in
        let args = List.map (fun e -> codegen_expr e bldr) exprList in
        build_call funLLValue args "calltmp" bldr
    | EArray (aName, idxExpr) -> 
        let baseVal = locate_llval env aName in
        let offset = codegen_expr idxExpr bldr in
        let gepExpr = build_gep baseVal [|offset|] "tmp_access" bldr in
            build_load gepExpr "tmp_load" bldr
    | EUnary (unaryOp, uExpr) ->
        let exprLLVal = codegen_expr uExpr env bldr in
        let exprValType = type_of exprLLVal in
        (
        match unaryOp with
        | UnaryRef -> build_gep exprLLVal [|int_type 0; int_type 0|] "tmp_ref" bldr 
        | UnaryDeref ->
            let ptrLLVal = build_gep exprLLVal [|int_type 0; int_type 0|] "tmp_ref" bldr in
            build_load ptrLLVal "tmp_load" bldr
        | UnaryPlus -> exprLLVal
        | UnaryMinus -> const_neg exprLLVal
        | UnaryNot -> const_not exprLLVal
        )
    | EBinOp (binOp, opand1, opand2) ->
        let llVal1 = codegen_expr env opand1 bldr in
        let llValType = type_of llVal1 in
        let llVal2 = codegen_expr env opand2 bldr in
        let build_fun, strng = 
        (
        match binOp with
        | BinDiv -> 
            if (llValType = int_type) then build_sdiv, "tmp_div"
            else build_fdiv, "tmp_div"
        | BinMulti -> 
            if (llValType = int_type) then build_mul, "tmp_mul"
            else build_fmul, "tmp_mul" 
        | BinMod ->
            if (llValType = int_type) then build_srem, "tmp_mod" 
            else build_frem, "tmp_mod" 
        | BinPlus ->
            if (llValType = int_type) then build_add, "tmp_add"
            else build_fadd, "tmp_add" 
        | BinMinus ->
            if (llValType = int_type) then build_sub, "tmp_sub"
            else build_fsub, "tmp_sub"
        | BinLess ->
            if (llValType = int_type) then build_icmp Slt, "tmp_less"
            else build_fcmp Slt, "tmp_less"
        | BinGreater ->
            if (llValType = int_type) then build_icmp Sgt, "tmp_greater"
            else build_fcmp Sgt, "tmp_greater"
        | BinLessEq ->
            if (llValType = int_type) then build_icmp Sle, "tmp_lesseq"
            else build_fcmp Sle, "tmp_lesseq"
        | BinGreaterEq ->
            if (llValType = int_type) then build_icmp Sge, "tmp_greatereq"
            else build_fcmp Sge, "tmp_greatereq"
        | BinEq ->
            if (llValType = int_type) then build_icmp Eq, "tmp_eq"
            else build_fcmp Eq, "tmp_eq"
        | BinNotEq -> 
            if (llValType = int_type) then build_icmp Ne, "tmp_noteq"
            else build_fcmp Ne, "tmp_noteq"
        | BinAnd ->
            build_and, "tmp_and"
        | BinOr ->
            build_or, "tmp_or"
        | BinComma -> 
            build_or, "" (*dummy func*)
        )
        in
        if strng <> "" then build_fun llVal1 llVal2 strng bldr
        else llVal2
    | EUnAssign (unAssOp, opLocation, expr) -> ()
    | EBinAssign (binAssOp, expr1, expr2) -> ()
    | ECast (OType (bType, pointerCnt) as objType, expr) -> ()
    | EConditional (exprCondition, exprTrue, exprFalse) -> ()
    | ENew (OType(basicType, pointerCnt), arrayOption) -> ()
    | ENewP (OType(bType, pointerCnt), newExprOption) -> ()
    | EDelete expr -> ()

and locate_llval env name = 
    try
        Hashtbl.find env name
    with
    | Not_found -> 
            let resOption = lookup_global name llmod in
            match resOption with
            | Some x -> x
            | None -> Printf.printf "\x1b[31mError\x1b[0m: Couldn't locate %s during generation.\n" name; exit 1

and get_llvm_type bType cnt =  (*NOTE(achilles): this could be slow as is...*)
    match cnt with
    | 0 ->
        (
            match bType with
            | TVoid -> non_type
            | TInt -> int_type
            | TChar -> char_type
            | TBool -> bool_type
            | TDouble -> double_type
        )
    | _ as num ->
        pointer_type (get_llvm_type bType (num - 1))

and make_param_array paramOption = 
    match paramOption with
    | None -> [||]
    | Some paramList ->
            let aux x = 
            (
                match x with
                | Param (xType, xName) ->
                        let OType(bType, pointerCnt) = xType in
                        get_llvm_type bType pointerCnt
                | ParamByRef (xType, xName) ->
                        let OType(bType, pointerCnt) = xType in
                        get_llvm_type bType (pointerCnt + 1)
            )
            in
            let llTypedParams = List.map aux paramList in
            Array.of_list llTypedParams

