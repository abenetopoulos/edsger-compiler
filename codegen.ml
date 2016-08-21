(*TODO:
  - make binary and/or short-circuit
  - currently we don't handle string arguments properly. They need to be stored as constants, and then
      passed to functions as pointers to char (verify_module warns about this)
  - when a function returns void, there shouldn't be a name to the left of the function call
    verify_module: "Instruction has a name, but provides a void value!"
  - change all string arguments to use a counter (to get output like clang's) {lowest priority possible}
*)

open Llvm
open Ast

exception Terminate of string

let llctx = global_context ()

let llm = create_module llctx "program_module"

let int_type = i16_type llctx
let char_type = i8_type llctx
let double_type = x86fp80_type llctx
let bool_type = i1_type llctx
let non_type = void_type llctx

type scope_type = SGlobal | SInternal

let rec generate_code node scope envOpt bldr =
    match node with
    | VarDecl (OType(bType, pointerCnt), declList) ->
        let basellType = get_llvm_type bType pointerCnt in
        let extract_additional x =
            let ADeclarator(xName, xExprOption) = x in
            let constantInt, llValInt = 
            (match scope with
             | SGlobal ->
                (match xExprOption with
                 | None -> 0, None
                 | Some ival -> 
                    (match ival with
                     | EInt iv -> iv, None
                     | _ -> raise (Terminate "Size specifier at global scope must be a constant integer expression")
                    )     
                )
             | SInternal ->
                (match xExprOption with
                 | None -> -1, None
                 | Some expr -> 
                    let env = 
                        (match envOpt with
                         | Some e -> e
                         | None -> raise (Terminate "An environment should have been specified at this point")
                        )
                    in
                    let tempVal = codegen_expr expr env bldr in
                    let finalVal = 
                    (match expr with
                     | EId _
                     | EArray _ -> build_load tempVal "tmp_load" bldr
                     | _ -> tempVal
                    ) in
                    -1, Some finalVal
                )
            ) in
            xName, constantInt, llValInt
        in
        let additionalllTypeOptions = List.map extract_additional declList in
        if (scope = SGlobal) then
            List.iter (fun (vName, additional, _) -> 
                match additional with 
                | 0 -> ignore (declare_global basellType vName llm)
                | _ as nItems -> ignore (declare_global (array_type basellType nItems) vName llm)
                ) additionalllTypeOptions
        else
            let env = 
                (match envOpt with
                 | Some e -> e
                 | None -> raise (Terminate "An environment should have been specified at this point")
                )
            in
            List.iter (fun (vName, _, additional) -> 
                let llval = 
                    match additional with 
                    | None -> (build_alloca basellType vName bldr)
                    | Some nItems -> (build_array_alloca basellType nItems vName bldr)
                in
                Hashtbl.add env vName llval
                ) additionalllTypeOptions
    | FunDecl (OType(bType, pointerCnt), name, paramOption) ->
        let llType = get_llvm_type bType pointerCnt in
        let _, paramArray = make_param_array paramOption in
        let fType = function_type llType paramArray in
        ignore (declare_function name fType llm)
    | FunDef(OType(bType, pointerCnt), name, paramOption, decls, stmts) ->
        let llType = get_llvm_type bType pointerCnt in
        let nameArray, paramArray = make_param_array paramOption in
        let fType = function_type llType paramArray in
        let func = define_function name fType llm in
        let _ = position_at_end (entry_block func) bldr in
        let env = 
            (match envOpt with
             | None -> 
                let e:(string, llvalue) Hashtbl.t = Hashtbl.create (List.length decls) in
                e
             | Some e -> Hashtbl.copy e 
             (*get copy. any declarations local to the new function will not affect the 
              * parent, but the child has access to the parent's decls*)
            )
        in
        let labels:(string, llbasicblock) Hashtbl.t = Hashtbl.create (List.length stmts) in (*no label sharing across units*)
        let _ = 
            Array.iteri (fun i a ->
                let n = nameArray.(i) in
                set_value_name n a;
                Hashtbl.add env n a) (params func)
        in
        let _ = List.iter (fun d -> 
                                let insBlock = insertion_block bldr in 
                                generate_code d SInternal (Some env) bldr;
                                position_at_end insBlock bldr) decls in
        let _ = List.iter (fun d -> codegen_stmt d env labels bldr) stmts in
        if (llType = non_type) then
            ignore (build_ret_void bldr)
        else
            ()
        
and codegen_stmt stmt env labels bldr = 
    match stmt with
    | SExpr expr -> ignore (codegen_expr expr env bldr)
    | SBlock stmts -> ignore (List.hd (List.rev (List.map (fun d -> codegen_stmt d env labels bldr) stmts)))
    | SIf (condExpr, stmt) ->
        let conditionLLVal = codegen_expr condExpr env bldr in
        let conditionLLVal =
        (match condExpr with
         | EId _ -> build_load conditionLLVal "tmp_load" bldr
         | _ -> conditionLLVal
        ) in
        let startBB = insertion_block bldr in
        let parentFunction = block_parent startBB in
        let thenBB = append_block llctx "then" parentFunction in
        position_at_end thenBB bldr;

        (*generate_code stmt SInternal bldr;*)
        ignore (codegen_stmt stmt env labels bldr);
        let newThenBB = insertion_block bldr in
        let mergeBB = append_block llctx "if_cont" parentFunction in
        position_at_end startBB bldr;

        ignore (build_cond_br conditionLLVal thenBB mergeBB bldr);
        position_at_end newThenBB bldr;
        ignore (build_br mergeBB bldr);

        position_at_end mergeBB bldr
    | SIfElse (condExpr, stmt1, stmt2) -> 
        let conditionLLVal = codegen_expr condExpr env bldr in
        let conditionLLVal =
        (match condExpr with
         | EId _ -> build_load conditionLLVal "tmp_load" bldr
         | _ -> conditionLLVal
        ) in
        let startBB = insertion_block bldr in
        let parentFunction = block_parent startBB in
        let thenBB = append_block llctx "then" parentFunction in
        position_at_end thenBB bldr;

        (*generate_code stmt1 SInternal bldr;*)
        ignore (codegen_stmt stmt1 env labels bldr);
        let newThenBB = insertion_block bldr in
        let elseBB = append_block llctx "else" parentFunction in
        position_at_end elseBB bldr;

        (*generate_code stmt2 SInternal bldr;*)
        ignore (codegen_stmt stmt2 env labels bldr);
        let newElseBB = insertion_block bldr in
        let mergeBB = append_block llctx "if_cont" parentFunction in
        position_at_end startBB bldr;

        ignore (build_cond_br conditionLLVal thenBB elseBB bldr);
        position_at_end newThenBB bldr;
        ignore (build_br mergeBB bldr);
        position_at_end newElseBB bldr;
        ignore (build_br mergeBB bldr);

        position_at_end mergeBB bldr

    | SFor (labelOption, initialization, condition, afterthought, stmt) ->
        (*let initializationLLVal = codegen_expr initialization env bldr in*)
        (match initialization with
         | None -> ()
         | Some i -> ignore (codegen_expr i env bldr)
        );
        let preheaderBB = insertion_block bldr in
        let parentFunction = block_parent preheaderBB in
        let loopBB = append_block llctx "tmp_loop" parentFunction in
        ignore (build_br loopBB bldr);

        position_at_end loopBB bldr;
        (match condition with
         | None -> ()
         | Some c ->
            let conditionLLVal = codegen_expr c env bldr in
            ignore
            (match c with
             | EId _ -> build_load conditionLLVal "tmp_load" bldr
             | _ -> conditionLLVal
            )
        );
        let bodyBB = insertion_block bldr in
        let thenBB = append_block llctx "body" parentFunction in
        position_at_end thenBB bldr;

        (*generate_code stmt SInternal bldr;*)
        ignore (codegen_stmt stmt env labels bldr);

        let afterthoughtBB = append_block llctx "tmp_afterthought" parentFunction in
        position_at_end afterthoughtBB bldr;
        (match afterthought with
         | None -> ()
         | Some a -> 
            ignore (codegen_expr a env bldr);
        );
        ignore (build_br loopBB bldr);

        let mergeBB = append_block llctx "merge" parentFunction in
        position_at_end mergeBB bldr;

        (match labelOption with
         | None -> ()
         | Some labelStrn ->
            let continueStrn = labelStrn ^ "Cont" in
            let breakStrn = labelStrn ^ "Break" in
            begin
                Hashtbl.add labels continueStrn afterthoughtBB;
                Hashtbl.add labels breakStrn mergeBB
            end
        )
    | SContinue labelOption ->
        (match labelOption with
         | Some labelStrn ->
            let targetLLVal = Hashtbl.find labels (labelStrn ^ "Cont") in
            ignore (build_br targetLLVal bldr)
         | None ->
            let currentBB = insertion_block bldr in 
            (match (block_pred currentBB) with
             | After prev -> ignore (build_br prev bldr)
             | _ -> raise (Terminate "this is wrong")
            )
         )
    | SBreak labelOption ->
        (match labelOption with
         | Some labelStrn ->
            let targetLLVal = Hashtbl.find labels (labelStrn ^ "Break") in
            ignore (build_br targetLLVal bldr)
         | None ->
            let currentBB = insertion_block bldr in 
            (match (block_succ currentBB) with
             | Before next -> ignore (build_br next bldr)
             | _ -> raise (Terminate "this is wrong")
            )
         )
    | SReturn exprOption ->
        ignore (match exprOption with
         | None -> build_ret_void bldr
         | Some expr ->
            let llValExpr = codegen_expr expr env bldr in
            (match expr with
             | EId _ 
             | EArray _ -> 
                let llValRet = build_load llValExpr "tmp_load" bldr in
                build_ret llValRet bldr
             | _ -> build_ret llValExpr bldr
            )
        )
    | _ -> ()

and codegen_expr expr env bldr =  
    match expr with
    | EId name -> locate_llval env name
    | EExpr nExpr -> codegen_expr nExpr env bldr
    | EBool b -> 
            let x = if b = true then 1 else 0 in 
            const_int bool_type x
    | EInt i -> const_int int_type i
    | EChar c -> 
            let ascii = Char.code c in
            const_int char_type ascii
    | EDouble d -> const_float double_type d
    | EString s -> const_stringz llctx s
    | ENull -> const_null int_type  (*NOTE: type is irrelevant to us*)
    | EFCall (fName, exprList) ->
        (match lookup_function fName llm with
         | None -> raise (Terminate "Function couldn't be found?")
         | Some funLLValue ->
            let args = 
                (match exprList with 
                 | None -> [||]
                 | Some eList -> Array.of_list (List.map (fun e -> codegen_expr e env bldr) eList) 
                )
            in
            build_call funLLValue args "calltmp" bldr
        )
    | EArray (aExpr, ArrExp idxExpr) -> 
        let exprLLV = codegen_expr aExpr env bldr in
        let baseVal = exprLLV in (*aExpr is either an identifier or a function call? either way, we shouldn't load it I think*)
        (*let baseVal = locate_llval env aName in*)
        let offset = 
            let partial = codegen_expr idxExpr env bldr in
            (match idxExpr with
             | EId _ | EArray _ -> build_load partial "tmp_load" bldr
             | _ -> partial
            ) 
        in
        let gepExpr = build_gep baseVal [|offset|] "tmp_access" bldr in
        gepExpr
    | EUnary (unaryOp, uExpr) ->
        let exprLLVal = codegen_expr uExpr env bldr in
        let exprValType = type_of exprLLVal in
        let constZero = const_int int_type 0 in
        (match unaryOp with
         | UnaryRef -> build_gep exprLLVal [|constZero; constZero|] "tmp_ref" bldr 
         | UnaryDeref ->
             let ptrLLVal = build_gep exprLLVal [|constZero; constZero|] "tmp_ref" bldr in
             build_load ptrLLVal "tmp_load" bldr
         | UnaryPlus -> exprLLVal
         | UnaryMinus -> 
            (match uExpr with
             | EId _
             | EArray _ ->
                let loadedVal = build_load exprLLVal "tmp_load" bldr in
                build_neg loadedVal "tmp_neg" bldr
             | _ -> build_neg exprLLVal "tmp_neg" bldr
            )
         | UnaryNot -> 
            (match uExpr with
             | EId _
             | EArray _ ->
                let loadedVal = build_load exprLLVal "tmp_load" bldr in
                build_not loadedVal "tmp_not" bldr
             | _ -> build_not exprLLVal "tmp_not" bldr
            )
        )
    | EBinOp (binOp, opand1, opand2) ->
        let llVal1 = codegen_expr opand1 env bldr in
        let llValType = type_of llVal1 in
        let llVal2 = codegen_expr opand2 env bldr in
        let build_fun, llVal2, strng =
        (match binOp with
         | BinDiv -> 
             if (llValType = int_type) then build_sdiv, llVal2, "tmp_div"
             else build_fdiv, llVal2, "tmp_div"
         | BinMulti -> 
             if (llValType = int_type) then build_mul, llVal2, "tmp_mul"
             else build_fmul, llVal2, "tmp_mul" 
         | BinMod ->
             if (llValType = int_type) then build_srem, llVal2, "tmp_mod" 
             else build_frem, llVal2, "tmp_mod" 
         | BinPlus ->
             if (llValType = int_type) then build_add, llVal2, "tmp_add" 
             else if (size_of llValType) = (size_of (pointer_type int_type)) then 
                let retVal = build_gep llVal1 [|llVal2|] "tmp_ptradd" bldr in
                build_add, retVal, "" (*dummy func*)
             else build_fadd, llVal2, "tmp_add" 
         | BinMinus ->
             if (llValType = int_type) then build_sub, llVal2, "tmp_sub"
             else if (size_of llValType) = (size_of (pointer_type int_type)) then
                 let negllVal2 = build_neg llVal2 "tmp_neg" bldr in
                 let retVal = build_gep llVal1 [|negllVal2|] "tmp_ptradd" bldr in
                 build_sub, retVal, "" (*dummy func*)
             else build_fsub, llVal2, "tmp_sub"
         | BinLess ->
             if (llValType = int_type || (size_of llValType) = (size_of (pointer_type int_type))) then build_icmp Icmp.Slt, llVal2, "tmp_less"
             else build_fcmp Fcmp.Olt, llVal2, "tmp_less"
         | BinGreater ->
             if (llValType = int_type || (size_of llValType) = (size_of (pointer_type int_type))) then build_icmp Icmp.Sgt, llVal2, "tmp_greater"
             else build_fcmp Fcmp.Ogt, llVal2, "tmp_greater"
         | BinLessEq ->
             if (llValType = int_type || (size_of llValType) = (size_of (pointer_type int_type))) then build_icmp Icmp.Sle, llVal2, "tmp_lesseq"
             else build_fcmp Fcmp.Ole, llVal2, "tmp_lesseq"
         | BinGreaterEq ->
             if (llValType = int_type || (size_of llValType) = (size_of (pointer_type int_type))) then build_icmp Icmp.Sge, llVal2, "tmp_greatereq"
             else build_fcmp Fcmp.Oge, llVal2, "tmp_greatereq"
         | BinEq ->
             if (llValType = int_type || (size_of llValType) = (size_of (pointer_type int_type))) then build_icmp Icmp.Eq, llVal2, "tmp_eq"
             else build_fcmp Fcmp.Oeq, llVal2, "tmp_eq"
         | BinNotEq -> 
             if (llValType = int_type || (size_of llValType) = (size_of (pointer_type int_type))) then build_icmp Icmp.Ne, llVal2, "tmp_noteq"
             else build_fcmp Fcmp.One, llVal2, "tmp_noteq"
         | BinAnd ->
             build_and, llVal2, "tmp_and"
         | BinOr ->
             build_or, llVal2, "tmp_or"
         | BinComma -> 
             build_or, llVal2, "" (*dummy func*)
        )
        in
        if strng <> "" then build_fun llVal1 llVal2 strng bldr
        else llVal2
    | EUnAssign (unAssOp, opLocation, expr) -> 
        let llValExpr = codegen_expr expr env bldr in
        let llValExprLoad = build_load llValExpr "tmp_load" bldr in
        let oneConst = const_int int_type 1 in
        let modifiedLLValExpr = 
        (match unAssOp with
         | UnaryPlusPlus -> build_add llValExprLoad oneConst "tmp_inc" bldr
         | UnaryMinusMinus -> build_sub llValExprLoad oneConst "tmp_inc" bldr
        ) in
        let res = build_store modifiedLLValExpr llValExpr bldr in
        (match opLocation with
         | LocRight -> llValExpr 
         | LocLeft -> res
        ) (*NOTE: most likely wrong. will crash and burn. avoid at all costs*)
    | EBinAssign (binAssOp, expr1, expr2) -> 
        let llVal1 = codegen_expr expr1 env bldr in
        let llVal2 = codegen_expr expr2 env bldr in
        let rightHandLLVal = 
            (match binAssOp with
            | BinAssign -> llVal2
            | BinAssignMulti -> codegen_expr (EBinOp (BinMulti, expr1, expr2)) env bldr
            | BinAssignDiv -> codegen_expr (EBinOp (BinDiv, expr1, expr2)) env bldr
            | BinAssignMod -> codegen_expr (EBinOp (BinMod, expr1, expr2)) env bldr
            | BinAssignPlus -> codegen_expr (EBinOp (BinPlus, expr1, expr2)) env bldr
            | BinAssignMinus -> codegen_expr (EBinOp (BinMinus, expr1, expr2)) env bldr
            ) 
        in
        build_store rightHandLLVal llVal1 bldr
    | EConditional (exprCondition, exprTrue, exprFalse) -> 
        let conditionLLVal = 
            let generated = codegen_expr exprCondition env bldr in
            (match exprCondition with
             | EId _ 
             | EArray _ -> build_load generated "tmp_load" bldr
             | _ -> generated
            )
        in
        let llVal1 = codegen_expr exprTrue env bldr in
        let llVal2 = codegen_expr exprFalse env bldr in
        build_select conditionLLVal llVal1 llVal2 "tmp_conditional" bldr
    | ENew (OType(basicType, pointerCnt), arrayOption) ->
        let llValType = get_llvm_type basicType pointerCnt in
        let countLLVal = 
        (match arrayOption with
         | None -> const_int int_type 1
         | Some (ArrExp exp) -> codegen_expr exp env bldr
        ) in
        let sizeOf = size_of llValType in
        let sizeLLVal = build_mul sizeOf countLLVal "tmp_size" bldr in
        (match lookup_function "new" llm with
         | None -> raise (Terminate "Couldn't locate 'new'")
         | Some funLLValue ->
            build_call funLLValue [|sizeLLVal|] "tmp_new" bldr
        )
    | ENewP (OType(bType, pointerCnt), newExprOption) ->
        let llValType = get_llvm_type bType pointerCnt in
        let countLLVal = 
        (match newExprOption with
        | Some (Some (ArrExp exp), None) -> codegen_expr exp env bldr
        | None -> const_int int_type 1
        | _ -> raise (Terminate "This shouldn't have happened")
        ) in
        let sizeOf = size_of llValType in
        let sizeLLVal = build_mul sizeOf countLLVal "tmp_size" bldr in
        (match lookup_function "new" llm with
         | None -> raise (Terminate "Couldn't locate 'new'")
         | Some funLLValue ->
            build_call funLLValue [|sizeLLVal|] "tmp_new" bldr (*NOTE: the function arg might be problematic...*)
        )
    | EDelete exp -> 
        let llValExpr = codegen_expr exp env bldr in
        let pointerToDestroy = build_load llValExpr "tmp_load" bldr in
        (match lookup_function "dispose" llm with
         | None -> raise (Terminate "Couldn't locate dispose in symbol table")
         | Some funLLValue ->
             build_call funLLValue [|pointerToDestroy|] "tmp_delete" bldr
        )
    | ECast (OType (bType, pointerCnt), expr) ->
        let llTargetType = get_llvm_type bType pointerCnt in
        let exprLLVal = codegen_expr expr env bldr in
        let llSourceType = type_of exprLLVal in
        let convResult = 
            if llTargetType = int_type then
                if llSourceType = double_type then
                    build_fptosi exprLLVal int_type "tmp_fptosi" bldr
                else if llSourceType = char_type then
                    build_zext exprLLVal int_type "tmp_chartosi" bldr
                else if llSourceType = bool_type then
                    build_zext exprLLVal int_type "tmp_booltosi" bldr
                else 
                    (match expr with
                     | EId _ 
                     | EArray _ -> build_load exprLLVal "tmp_load" bldr
                     | _ -> exprLLVal
                    )
            else if llTargetType = double_type then
                if llSourceType = int_type then
                    build_sitofp exprLLVal double_type "tmp_sitofp" bldr
                else if llSourceType = char_type then
                    build_uitofp exprLLVal double_type "tmp_chartofp" bldr
                else if llSourceType = bool_type then
                    build_uitofp exprLLVal double_type "tmp_booltofp" bldr
                else 
                    (match expr with
                     | EId _ 
                     | EArray _ -> build_load exprLLVal "tmp_load" bldr
                     | _ -> exprLLVal
                    )
            else if llTargetType = char_type then
                if llSourceType = int_type then
                    build_trunc exprLLVal char_type "tmp_sitochar" bldr 
                else if llSourceType = double_type then
                    build_fptoui exprLLVal char_type "tmp_fptochar" bldr
                else if llSourceType = bool_type then
                    build_zext exprLLVal char_type "tmp_booltochar" bldr
                else 
                    (match expr with
                     | EId _ 
                     | EArray _ -> build_load exprLLVal "tmp_load" bldr
                     | _ -> exprLLVal
                    )
            else if llTargetType = bool_type then
                if llSourceType = double_type then
                    let constZero = const_float double_type 0.0 in 
                    build_fcmp Fcmp.One exprLLVal constZero "tmp_fptobool" bldr
                else if llSourceType = char_type || llSourceType = int_type then
                    let constZero = const_int int_type 0 in 
                    build_icmp Icmp.Ne exprLLVal constZero "tmp_itobool" bldr
                else 
                    (match expr with
                     | EId _ 
                     | EArray _ -> build_load exprLLVal "tmp_load" bldr
                     | _ -> exprLLVal
                    )
            else (*this will match casts between pointer types*)
                let bitcastNeeded = (llTargetType <> llSourceType) in (*this comparison might not work...*)
                let actualLLVal =
                (match expr with
                 | EId _ 
                 | EUnary(UnaryDeref, _) -> build_load exprLLVal "tmp_load" bldr
                 | EUnary(UnaryRef, _) -> exprLLVal (*should do nothing, since the exprLLVal is the result of getelementpointer?*)
                 | _ -> exprLLVal
                ) in
                if bitcastNeeded then 
                    build_bitcast actualLLVal llTargetType "tmp_ptrtoptr" bldr
                else
                    actualLLVal
        in
        convResult

and locate_llval env name = 
    try
        Hashtbl.find env name
    with
    | Not_found -> 
        let resOption = lookup_global name llm in
        match resOption with
        | Some x -> x
        | None -> 
            Printf.printf "\x1b[31mError\x1b[0m: Couldn't locate %s during generation.\n" name;
            dump_module llm;
            exit 1
                        

and get_llvm_type bType cnt =
    match cnt with
    | 0 ->
        (match bType with
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
    | None -> [||], [||]
    | Some paramList ->
        let aux x = 
        (match x with
            | Param (xType, xName) ->
                    let OType(bType, pointerCnt) = xType in
                    xName, get_llvm_type bType pointerCnt
            | ParamByRef (xType, xName) ->
                    let OType(bType, pointerCnt) = xType in
                    xName, get_llvm_type bType (pointerCnt + 1)
        )
        in
        let llTypedParams = List.map aux paramList in
        let names = List.map (fun (a, _) -> a) llTypedParams in
        let types = List.map (fun (_, b) -> b) llTypedParams in
        Array.of_list names, Array.of_list types

let code_gen ast =
    match ast with
    | [] -> raise (Terminate "AST is empty")
    | _ as tree ->
        let bldr = builder llctx in
        let _ = List.iter (fun x -> generate_code x SGlobal None bldr) tree in
        llm
