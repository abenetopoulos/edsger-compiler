(*TODO:
  - test everything!
  - change all string arguments to use a counter (to get output like clang's) {lowest priority possible}
*)

(*NOTE: 
  - changed what unary deref returns, might cause issues, keep in mind [6/9/16]
 * *)

(*NOTE: Things which won't work:
   - anything involving labels
 * *)

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

(*let extraArgs:(string, llvalue array) Hashtbl.t = Hashtbl.create 100*)
let extraArgs:(string, string array) Hashtbl.t = Hashtbl.create 100

type scope_type = SGlobal | SInternal

let sort_and_remove_dups l =
    let sort_func a b = 
        if (a = b) then 0
        else if (a < b) then -1
        else 1
    in
    let rec aux lastElement l acc = 
        match l with
        | [] -> List.rev acc
        | h :: t -> 
            if h = lastElement then
                aux lastElement t acc
            else
                aux h t (h::acc)
    in
    let sorted = List.sort sort_func l in
    match sorted with
    | [] -> []
    | _ -> 
        let h = List.hd sorted in
        aux h (List.tl sorted) [h]

let string_of_list l = 
    List.fold_left (fun acc s -> s ^ acc) "" l

let rec generate_code node scope envOpt parentFuncStrList tripleOpt bldr =
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
                    let tempVal = codegen_expr expr env parentFuncStrList bldr in
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
        if (scope = SGlobal) then begin
            let llType = get_llvm_type bType pointerCnt in
            let _, paramArray = make_param_array paramOption in
            let fType = function_type llType paramArray in
            ignore (declare_function name fType llm)
        end
        else
            ()
    | FunDef(OType(bType, pointerCnt), name, paramOption, decls, stmts) ->
        let llType = get_llvm_type bType pointerCnt in
        let nameArray, baseParamArray = make_param_array paramOption in
        (*let env, extraNames, extraValues, extraTypes = 
            (match envOpt with
             | None -> 
                let e:(string, llvalue) Hashtbl.t = Hashtbl.create (List.length decls) in
                e, [||], [||], [||]
             | Some e -> 
                let newEnv:(string, llvalue) Hashtbl.t = Hashtbl.create (List.length decls) in
                let n, v, t = generate_triple e (string_of_list parentFuncStrList) bldr in
                newEnv, n, v, t
            )
        in*)
        let env:(string, llvalue) Hashtbl.t = Hashtbl.create (List.length decls) in
        let extraNames, extraValues, extraTypes = 
            (match tripleOpt with
             | None -> [||], [||], [||]
             | Some (n, v, t) -> n, v, t
            )
        in
        (*let extraNames, extraValues, extraTypes = generate_triple env in*)
        let nameArray = Array.append nameArray extraNames in
        let paramArray = Array.append baseParamArray extraTypes in
        (*let paramArray = baseParamArray in*)
        let fType = function_type llType paramArray in
        (*
        let paramString = 
            match paramOption with
            | None -> "N"
            | Some params -> get_param_string params
        in
        let parentFuncName =
            match parentOpt with
            | None -> ""
            | Some pN -> pN ^ "_"
        in
        let id = "_" ^ parentFuncName ^ name ^ "_" ^ paramString in
        *)
        let parentFuncStr = (string_of_list parentFuncStrList) in
        let id = 
            if scope = SGlobal then name
            else "_" ^ parentFuncStr ^ name 
        in
        let _ = 
            (match (lookup_function id llm) with
             | Some f -> delete_function f
             | None -> ()
            )
        in
        let func = define_function id fType llm in
        let newFuncStrList = name :: parentFuncStrList in
        (*let _ = Hashtbl.add extraArgs id extraValues in*)
        (*let _ = Hashtbl.add extraArgs id extraNames in*)
        let _ = position_at_end (entry_block func) bldr in
        let labels:(string, llbasicblock) Hashtbl.t = Hashtbl.create (List.length stmts) in (*no label sharing across units*)
        let _ = 
            Array.iteri (fun i a ->
                let n = nameArray.(i) in
                set_value_name n a;
                let aType = type_of a in
                let llStack = build_alloca aType n bldr in
                let storedLL = build_store a llStack bldr in
                Hashtbl.add env n llStack) (params func)
        in
        (*let _ = List.iter (fun d -> 
                                let insBlock = insertion_block bldr in 
                                generate_code d SInternal (Some env) newFuncStrList bldr;
                                position_at_end insBlock bldr) decls in*)
        let funcDefList = List.fold_left (fun acc d -> 
                                    match d with
                                    | FunDef _ as fd -> fd :: acc
                                    | _ as d -> 
                                        let insBlock = insertion_block bldr in 
                                        generate_code d SInternal (Some env) newFuncStrList None bldr;
                                        position_at_end insBlock bldr;
                                        acc) [] decls in
        let triple = generate_triple env (string_of_list newFuncStrList) bldr in
        let _ = generate_func_declarations funcDefList (string_of_list newFuncStrList) triple (*bldr*) in
        let _ = List.iter (fun d ->
                                let insBlock = insertion_block bldr in
                                generate_code d SInternal (Some env) newFuncStrList (Some triple) bldr;
                                position_at_end insBlock bldr) (List.rev funcDefList) in
        let _ = List.iter (fun d -> codegen_stmt d env labels newFuncStrList bldr) stmts in
        if (llType = non_type) then
            ignore (build_ret_void bldr)
        else
            ()
        
and codegen_stmt stmt env labels parentFuncStrList bldr = 
    match stmt with
    | SExpr expr -> ignore (codegen_expr expr env parentFuncStrList bldr)
    | SBlock stmts -> ignore (List.hd (List.rev (List.map (fun d -> codegen_stmt d env labels parentFuncStrList bldr) stmts)))
    | SIf (condExpr, stmt) ->
        (*let thenBB, _, mergeBB = codegen_conditional_expr condExpr false None None env bldr in*)
        let thenBB, mergeBB = codegen_conditional_expr condExpr None None env parentFuncStrList bldr in
        position_at_end thenBB bldr;

        ignore (codegen_stmt stmt env labels parentFuncStrList bldr);
        let newThenBB = insertion_block bldr in
        position_at_end newThenBB bldr;
        ignore (build_br mergeBB bldr);
        position_at_end mergeBB bldr;
    | SIfElse (condExpr, stmt1, stmt2) -> 
        (*let thenBB, elseBBOpt, mergeBB = codegen_conditional_expr condExpr true None None env bldr in*)
        let thenBB,elseBB = codegen_conditional_expr condExpr None None env parentFuncStrList bldr in
        position_at_end thenBB bldr;

        ignore (codegen_stmt stmt1 env labels parentFuncStrList bldr);
        let newThenBB = insertion_block bldr in
        let parentFunction = block_parent newThenBB in
        position_at_end elseBB bldr;

        ignore (codegen_stmt stmt2 env labels parentFuncStrList bldr);

        let newElseBB = insertion_block bldr in
        let mergeBB = append_block llctx "merge" parentFunction in
        position_at_end newThenBB bldr;
        ignore (build_br mergeBB bldr);
        position_at_end newElseBB bldr;
        ignore(build_br mergeBB bldr);
        position_at_end mergeBB bldr;
    | SFor (labelOption, initialization, condition, afterthought, stmt) -> (*this case now [15/9] seems a little toxic...*)
        (match initialization with
         | None -> ()
         | Some i -> ignore (codegen_expr i env parentFuncStrList bldr)
        );
        let preheaderBB = insertion_block bldr in
        let parentFunction = block_parent preheaderBB in
        let loopBB = append_block llctx "tmp_loop" parentFunction in
        ignore (build_br loopBB bldr);

        position_at_end loopBB bldr;
        let conditionLLVal = 
        (match condition with
         | None -> None
         | Some c ->
            let conditionLLVal = codegen_expr c env parentFuncStrList bldr in
            let res = 
            (match c with
             | EId _ -> build_load conditionLLVal "tmp_load" bldr
             | _ -> conditionLLVal
            ) in
            Some res
        ) in
        let bodyBB = insertion_block bldr in
        let thenBB = append_block llctx "body" parentFunction in
        position_at_end thenBB bldr;

        ignore (codegen_stmt stmt env labels parentFuncStrList bldr);

        let afterthoughtBB = append_block llctx "tmp_afterthought" parentFunction in
        position_at_end afterthoughtBB bldr;
        (match afterthought with
         | None -> ()
         | Some a -> 
            ignore (codegen_expr a env parentFuncStrList bldr);
        );
        ignore (build_br loopBB bldr);

        position_at_end thenBB bldr;
        ignore (build_br afterthoughtBB bldr);
        let mergeBB = append_block llctx "merge" parentFunction in
        position_at_end loopBB bldr;
        (match conditionLLVal with
         | None -> ignore (build_br thenBB bldr)
         | Some c -> ignore (build_cond_br c thenBB mergeBB bldr);
        );
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
            (match expr with
             | ENull ->
                let currentBB = insertion_block bldr in
                let func = block_parent currentBB in
                let retType = return_type (element_type (type_of func)) in
                let nullExpr = const_null retType in
                build_ret nullExpr bldr
             | _ as e ->
                let llValExpr = codegen_expr expr env parentFuncStrList bldr in
                (match expr with
                 | EId _ 
                 | EArray _ -> 
                    let llValRet = build_load llValExpr "tmp_load" bldr in
                    build_ret llValRet bldr
                 | _ -> build_ret llValExpr bldr
                )
            )
        )
    | _ -> ()

and codegen_expr expr env parentFuncStrList bldr =  
    match expr with
    | EId name -> locate_llval env name bldr
    | EExpr nExpr -> codegen_expr nExpr env parentFuncStrList bldr
    | EBool b -> 
            let x = if b = true then 1 else 0 in 
            const_int bool_type x
    | EInt i -> const_int int_type i
    | EChar c -> 
            let ascii = Char.code c in
            const_int char_type ascii
    | EDouble d -> const_float double_type d
    | EString s -> 
        let stringConstInit = const_stringz llctx s in
        let stringConst = define_global ".str" stringConstInit llm in 
        let constZero = const_int int_type 0 in
        build_gep stringConst [|constZero; constZero|] "tmp_str" bldr
    | ENull -> const_null (pointer_type int_type)  (*NOTE: this is just a dummy, caller generates actual value with correct type*)
    | EFCall (fName, exprList) ->
        let rec aux l =
            let name, lNames = 
            (match l with
            | [] -> fName, []
            | _ as lN -> "_" ^ (string_of_list lN) ^ fName, lN
            )
            in
            (match lookup_function name llm with
             | None -> 
                if (l = []) then raise (Terminate "Function couldn't be found")
                else aux (List.tl l)
             | Some funLLValue ->
                let fType = element_type (type_of funLLValue) in
                let args = 
                (match exprList with 
                 | None -> [||]
                 | Some eList -> (*Array.of_list (process_args eList (param_types fType) env bldr)*)
                    let subArray = Array.sub (param_types fType) 0 (List.length eList) in
                    Array.of_list (process_args eList subArray env parentFuncStrList bldr)
                )
                in
                let args =
                if (Array.length (param_types fType) > Array.length args) then begin
                    let extraVals = 
                        let extraNames = Hashtbl.find extraArgs name in
                        let locate_val_in_env n =
                            try
                                let res = Hashtbl.find env n in
                                build_load res "tmp_ref_load" bldr
                            with
                            | Not_found ->
                                let cutoff = String.index_from n 1 '_' in
                                let baseName = String.sub n (cutoff + 4) (String.length n - (cutoff + 4)) in
                                try
                                    Hashtbl.find env baseName
                                with
                                | Not_found -> 
                                    Printf.printf "tried to locate %s, %d\n" baseName cutoff;
                                    raise (Terminate "Couldn't locate external dependency for call")
                        in
                        Array.map locate_val_in_env extraNames
                    in
                    Array.append args extraVals
                end
                else
                    args
                in
                let str = 
                    if (return_type fType = non_type) then
                        ""
                    else
                        "tmp_call"
                in (
                build_call funLLValue args str bldr
                )
            )
        in
        aux parentFuncStrList
    | EArray (aExpr, ArrExp idxExpr) -> 
        let exprLLV = codegen_expr aExpr env parentFuncStrList bldr in
        let baseVal = exprLLV in (*aExpr is either an identifier or a function call? either way, we shouldn't load it I think*)
        let offset = 
            let partial = codegen_expr idxExpr env parentFuncStrList bldr in
            (match idxExpr with
             | EId _ | EArray _ -> build_load partial "tmp_load" bldr
             | _ -> partial
            ) 
        in
        let gepExpr = build_gep baseVal [|offset|] "tmp_access" bldr in
        gepExpr
    | EUnary (unaryOp, uExpr) ->
        let exprLLVal = codegen_expr uExpr env parentFuncStrList bldr in
        let exprValType = type_of exprLLVal in
        let constZero = const_int int_type 0 in
        (match unaryOp with
         | UnaryRef -> exprLLVal (*all llVals which could be referenced are stored as pointers already...*)
                 (*build_gep exprLLVal [|constZero; constZero|] "tmp_ref" bldr*)
         | UnaryDeref ->
             (*let ptrLLVal = build_gep exprLLVal [|constZero; constZero|] "tmp_ref" bldr in*)
             let ptrLLVal = build_load exprLLVal "tmp_ptr" bldr in
             ptrLLVal
             (*build_load ptrLLVal "tmp_load" bldr*)
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
    | EBinOp (binOp, opand1, opand2) as binExpr ->
        (match binOp with
         | BinAnd ->
            let trueBB, falseBB = codegen_conditional_expr binExpr None None env parentFuncStrList bldr in
            let parentFunction = block_parent trueBB in
            let mergeBB = append_block llctx "merge" parentFunction in
            position_at_end falseBB bldr;

            ignore (build_br mergeBB bldr);
            position_at_end trueBB bldr;
            ignore (build_br mergeBB bldr);
            position_at_end mergeBB bldr;

            let constFalse = const_int bool_type 0 in
            let constTrue = const_int bool_type 1 in
            build_phi [(constFalse, falseBB); (constTrue, trueBB)] "tmp_phi" bldr
         | BinOr ->
            let trueBB, falseBB = codegen_conditional_expr binExpr None None env parentFuncStrList bldr in
            let parentFunction = block_parent trueBB in
            let mergeBB = append_block llctx "merge" parentFunction in
            position_at_end falseBB bldr;

            ignore (build_br mergeBB bldr);
            position_at_end trueBB bldr;
            ignore (build_br mergeBB bldr);
            position_at_end mergeBB bldr;

            let constFalse = const_int bool_type 0 in
            let constTrue = const_int bool_type 1 in
            build_phi [(constFalse, falseBB); (constTrue, trueBB)] "tmp_phi" bldr
         | _ as bop ->
            let llVal1 = codegen_expr opand1 env parentFuncStrList bldr in
            let llValType = 
            (match (classify_type (type_of llVal1)) with
             | Pointer -> element_type (type_of llVal1) 
             | _ -> type_of llVal1
            )
            in
            let llVal1 = 
            (match opand1 with
             | EId _
             | EArray _
             | EUnary(UnaryDeref, _) -> build_load llVal1 "tmp_load" bldr
             | _ -> llVal1
            )
            in
            let llVal2 = codegen_expr opand2 env parentFuncStrList bldr in
            let llVal1, llVal2 = 
            (match opand2 with
             | ENull -> 
               (match opand1 with
                | ENull -> const_null (pointer_type int_type), const_null (pointer_type int_type)
                | _ -> llVal1, const_null (type_of llVal1)
               )
             | EId _
             | EArray _ 
             | EUnary(UnaryDeref, _) -> 
                let l2 = build_load llVal2 "tmp_load" bldr in
                (match opand1 with
                 | ENull -> const_null (type_of l2), l2
                 | _ -> llVal1, l2
                )
             | _ -> 
                (match opand1 with
                 | ENull -> const_null (type_of llVal2), llVal2
                 | _ -> llVal1, llVal2
                )
            )
            in
            let build_fun, llVal2, strng =
            (match bop with
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
                 if (llValType = int_type || llValType = bool_type || (size_of llValType) = (size_of (pointer_type int_type))) then build_icmp Icmp.Eq, llVal2, "tmp_eq"
                 else build_fcmp Fcmp.Oeq, llVal2, "tmp_eq"
             | BinNotEq -> 
                 if (llValType = int_type || llValType = bool_type || (size_of llValType) = (size_of (pointer_type int_type))) then build_icmp Icmp.Ne, llVal2, "tmp_noteq"
                 else build_fcmp Fcmp.One, llVal2, "tmp_noteq"
                      | BinComma -> 
                 build_or, llVal2, "" (*dummy func*)
             | _ -> raise (Terminate "well, this is quite the failure!\n")
            )
            in
            if strng <> "" then build_fun llVal1 llVal2 strng bldr
            else llVal2
        )
    | EUnAssign (unAssOp, opLocation, expr) -> 
        let llValExpr = codegen_expr expr env parentFuncStrList bldr in
        let llValExprLoad = build_load llValExpr "tmp_load" bldr in
        let oneConst = const_int int_type 1 in
        let modifiedLLValExpr = 
        (match unAssOp with
         | UnaryPlusPlus -> build_add llValExprLoad oneConst "tmp_inc" bldr
         | UnaryMinusMinus -> build_sub llValExprLoad oneConst "tmp_inc" bldr
        ) in
        let res = build_store modifiedLLValExpr llValExpr bldr in
        (match opLocation with
         | LocRight -> llValExprLoad 
         | LocLeft -> modifiedLLValExpr
        )
    | EBinAssign (binAssOp, expr1, expr2) -> 
        let llVal1 = codegen_expr expr1 env parentFuncStrList bldr in
        let llVal2 = codegen_expr expr2 env parentFuncStrList bldr in
        let rightHandLLVal = 
            (match binAssOp with
            | BinAssign -> 
                (match expr2 with
                 | ENull -> const_null (element_type (type_of llVal1))
                 | EId _
                 | EArray _ 
                 | EUnary(UnaryDeref, _) -> build_load llVal2 "tmp_load" bldr
                 | _ -> llVal2
                )
            | BinAssignMulti -> codegen_expr (EBinOp (BinMulti, expr1, expr2)) env parentFuncStrList bldr
            | BinAssignDiv -> codegen_expr (EBinOp (BinDiv, expr1, expr2)) env parentFuncStrList bldr
            | BinAssignMod -> codegen_expr (EBinOp (BinMod, expr1, expr2)) env parentFuncStrList bldr
            | BinAssignPlus -> codegen_expr (EBinOp (BinPlus, expr1, expr2)) env parentFuncStrList bldr
            | BinAssignMinus -> codegen_expr (EBinOp (BinMinus, expr1, expr2)) env parentFuncStrList bldr
            ) 
        in
        build_store rightHandLLVal llVal1 bldr
    | EConditional (exprCondition, exprTrue, exprFalse) -> 
        let trueBB, falseBB = codegen_conditional_expr exprCondition None None env parentFuncStrList bldr in
        let parentFunc = block_parent trueBB in
        let mergeBB = append_block llctx "merge" parentFunc in
        position_at_end trueBB bldr;

        let llVal1 = codegen_expr exprTrue env parentFuncStrList bldr in
        let llVal1 =
        (match exprTrue with
         | EId _ 
         | EArray _ -> build_load llVal1 "tmp_load" bldr
         | _ -> llVal1
        )
        in
        ignore (build_br mergeBB bldr);

        position_at_end falseBB bldr;
        let llVal2 = codegen_expr exprFalse env parentFuncStrList bldr in
        let llVal2 =
        (match exprFalse with
         | EId _ 
         | EArray _ -> build_load llVal2 "tmp_load" bldr
         | _ -> llVal2
        )
        in
        ignore (build_br mergeBB bldr);

        position_at_end mergeBB bldr;
        build_phi [(llVal1, trueBB); (llVal2, falseBB)] "tmp_phi" bldr
    | ENew (OType(basicType, pointerCnt), arrayOption) ->
        let llValType = get_llvm_type basicType pointerCnt in
        let countLLVal = 
        (match arrayOption with
         | None -> const_int int_type 1
         | Some (ArrExp exp) -> codegen_expr exp env parentFuncStrList bldr
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
        | Some (Some (ArrExp exp), None) -> codegen_expr exp env parentFuncStrList bldr
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
        let llValExpr = codegen_expr exp env parentFuncStrList bldr in
        let pointerToDestroy = build_load llValExpr "tmp_load" bldr in
        (match lookup_function "dispose" llm with
         | None -> raise (Terminate "Couldn't locate dispose in symbol table")
         | Some funLLValue ->
             build_call funLLValue [|pointerToDestroy|] "tmp_delete" bldr
        )
    | ECast (OType (bType, pointerCnt), expr) ->
        let llTargetType = get_llvm_type bType pointerCnt in
        let exprLLVal = codegen_expr expr env parentFuncStrList bldr in
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

and locate_llval env name bldr = 
    try
        Hashtbl.find env name
    with
    | Not_found -> 
        (try
            let v = Hashtbl.find env ("_ref" ^ name) in
            build_load v "tmp_load" bldr
        with
        | Not_found ->
            let resOption = lookup_global name llm in
            match resOption with
            | Some x -> x
            | None -> 
                Printf.printf "\x1b[31mError\x1b[0m: Couldn't locate %s during generation.\n" name;
                dump_module llm;
                exit 1
        )
                        
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

and process_args eList paramTypeArray env parentFuncStrList bldr = 
    let paramTypeList = Array.to_list paramTypeArray in
    let aux e p = 
        let eLLVal = codegen_expr e env parentFuncStrList bldr in
        let eType = type_of eLLVal in
        if (eType = p) then eLLVal
        else
            build_load eLLVal "tmp_load" bldr
    in
    List.map2 (fun e p -> aux e p) eList paramTypeList

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
                    (*xName, get_llvm_type bType (pointerCnt + 1)*)
                    ("_ref" ^ xName), get_llvm_type bType (pointerCnt + 1)
        )
        in
        let llTypedParams = List.map aux paramList in
        let names = List.map (fun (a, _) -> a) llTypedParams in
        let types = List.map (fun (_, b) -> b) llTypedParams in
        Array.of_list names, Array.of_list types

and codegen_conditional_expr condExpr trueBBOption falseBBOption env parentFuncStrList bldr = (* caller is responsible for adding 
                                                                                               * branches at the end of basic blocks *)
    let aux e trueBBOption falseBBOption =
        let exprLLVal = codegen_expr e env parentFuncStrList bldr in
        let exprLLVal = 
            (match e with
             | ENull -> const_int bool_type 0
             | EId _
             | EArray _ -> 
                let loadedLLVal = build_load exprLLVal "tmp_load" bldr in
                let constZero = const_int bool_type 0 in
                build_icmp Icmp.Ne loadedLLVal constZero "tmp_cmp" bldr
             | EFCall _ ->
                let constZero = const_int bool_type 0 in
                build_icmp Icmp.Ne exprLLVal constZero "tmp_cmp" bldr
             | _ -> exprLLVal
            )
        in
        let startBB = insertion_block bldr in
        let parentFunction = block_parent startBB in
        let trueBB = 
        (match trueBBOption with
         | None -> append_block llctx "cond_true" parentFunction
         | Some bb -> bb
        ) in
        let falseBB =
        (match falseBBOption with
         | None -> append_block llctx "cond_false" parentFunction
         | Some bb -> bb
        ) in
        position_at_end startBB bldr;
        ignore (build_cond_br exprLLVal trueBB falseBB bldr);
        (trueBB, falseBB);
    in
    match condExpr with
    | EBinOp (binOp, opand1, opand2) as expr ->
        (match binOp with
         | BinAnd | BinOr ->
            let trueBB1, falseBB1 = 
            (match opand1 with
             | EBinOp(op1, _, _) as e ->
                if ((op1 = BinAnd) || (op1 = BinOr)) then
                    codegen_conditional_expr e trueBBOption falseBBOption env parentFuncStrList bldr
                else
                    if binOp = BinAnd then
                        aux e None falseBBOption
                    else
                        aux e trueBBOption None
             | _ as op1 ->
                if binOp = BinAnd then
                    aux op1 None falseBBOption
                else
                    aux op1 trueBBOption None
            ) in
            (); 

            if binOp = BinAnd then begin
                position_at_end trueBB1 bldr;
                let trueBB2, falseBB2 = 
                (match opand2 with
                 | EBinOp(op2, _, _) as e ->
                    if ((op2 = BinAnd) || (op2 = BinOr)) then
                        codegen_conditional_expr e trueBBOption (Some falseBB1) env parentFuncStrList bldr
                    else
                        aux e trueBBOption (Some falseBB1)
                 | _ as op2 ->
                    aux op2 trueBBOption (Some falseBB1)
                ) in
                (trueBB2, falseBB1) 
            end
            else begin
                position_at_end falseBB1 bldr;
                let trueBB2, falseBB2 =
                (match opand2 with
                 | EBinOp(op2, _, _) as e ->
                    if ((op2 = BinAnd) || (op2 = BinOr)) then
                        codegen_conditional_expr e (Some trueBB1) falseBBOption env parentFuncStrList bldr
                    else
                        aux e (Some trueBB1) falseBBOption
                 | _ as op2 ->
                    aux op2 (Some trueBB1) falseBBOption
                )
                in
                (trueBB1, falseBB2)
            end
         | _ ->
            aux expr trueBBOption falseBBOption
        )
    | _ as expr ->
        aux expr trueBBOption falseBBOption

and generate_triple env scopeString bldr = 
    let auxTbl:(string, llvalue * lltype) Hashtbl.t = Hashtbl.create (Hashtbl.length env) in
    let aux k v =
        if ((String.length k) > 4 && (String.sub k 0 4 = "_ref" || (String.sub k 0 4 <> "_ref" && k.[0] = '_'))) then (*NOTE: this check looks like shit, will probably perform like shit*)
            (*let valueToKeep = build_load v "tmp_load" bldr in*)
            let valueToKeep = v in
            Hashtbl.add auxTbl k (valueToKeep, element_type (type_of valueToKeep))
        else begin
            try
                let _ = Hashtbl.find auxTbl ("_ref" ^ k) in
                let _ = Hashtbl.remove auxTbl ("_ref" ^ k) in
                Hashtbl.add auxTbl ("_ref" ^ k) (v, type_of v)
            with
            | Not_found -> 
                Hashtbl.add auxTbl ("_" ^ scopeString ^ "_ref" ^ k) (v, type_of v)
        end
    in
    let _ = Hashtbl.iter aux env in 
    let a,b,c = Hashtbl.fold (fun k (v, t) (a, b, c) -> (k :: a, v :: b, t :: c)) auxTbl ([],[],[]) in
    (Array.of_list (List.rev a), Array.of_list (List.rev b), Array.of_list (List.rev c))

and generate_func_declarations fdefs parentFuncStr triple = 
    let extraNames, extraTypes = 
        match triple with
        | n, _, t -> n, t
        | _ -> raise (Terminate "why did this go to shit?\n")
    in
    let idPrefix = "_" ^ parentFuncStr in
    let aux d = 
        match d with
        | FunDef(OType(bType, pointerCnt), name, paramOption, _, _) ->
           let llType = get_llvm_type bType pointerCnt in
           let _, baseParamArray = make_param_array paramOption in
           let paramArray = Array.append baseParamArray extraTypes in
           let fType = function_type llType paramArray in
           let id = idPrefix ^ name in
           begin
           ignore (declare_function id fType llm);
           Hashtbl.add extraArgs id extraNames
           end
        | _ -> raise (Terminate "There is a non function in generate_func_declarations\n")
    in
    List.iter (fun d -> aux d) fdefs

let code_gen ast =
    match ast with
    | [] -> raise (Terminate "AST is empty")
    | _ as tree ->
        try
            let bldr = builder llctx in
            let _ = List.iter (fun x -> generate_code x SGlobal None [""] None bldr) tree in
            llm
        with
        | _ as e -> Printf.printf "\x1b[31mException\x1b[0m: %s\n" (Printexc.to_string e); dump_module llm; exit 1
