(*TODO:
  - test everything!
*)

(*NOTE: 
  - changed what unary deref returns, might cause issues, keep in mind [achilles, 6/9/16]
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

(*let extraArgs:(string, llvalue array) Hashtbl.t = Hashtbl.create 100*)
let extraArgs:(string, string array) Hashtbl.t = Hashtbl.create 100

type scope_type = SGlobal | SInternal
type branchInstr = ReturnBranch of llbasicblock 
                 | BreakBranch of llbasicblock * (string option) 
                 | ContinueBranch of llbasicblock * (string option)

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

let rec generate_code node scope envOpt arrayEnvOpt parentFuncStrList tripleOpt bldr =
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
                    let arrayEnv = 
                    (match arrayEnvOpt with
                     | Some e -> e
                     | None -> raise (Terminate "An environment should have been specified at this point")
                    )
                    in
                    let tempVal = codegen_expr expr env arrayEnv parentFuncStrList bldr in
                    let intVal, finalVal = 
                    (match expr with
                     (*| EInt integer -> integer, tempVal*)
                     | EId _
                     | EArray _ -> -1, build_load tempVal "tmp_load" bldr
                     | _ -> -1, tempVal
                    ) in
                    intVal, Some finalVal
                    (*-1, Some finalVal*)
                )
            ) in
            xName, constantInt, llValInt
        in
        let additionalllTypeOptions = List.map extract_additional declList in
        if (scope = SGlobal) then
            let constZero =
                if (basellType = double_type) then const_float double_type 0.0
                else
                    const_int basellType 0
            in
            List.iter (fun (vName, additional, _) -> 
                match additional with 
                | 0 -> ignore (define_global vName constZero llm)
                | _ as nItems -> 
                    (*let arr_type = array_type basellType nItems in*)
                    let initLLVal = const_array basellType (Array.make nItems constZero) in
                    ignore (define_global vName initLLVal llm)
                    (*ignore (declare_global (array_type basellType nItems) vName llm)*)
                ) additionalllTypeOptions
        else
            let env = 
            (match envOpt with
             | Some e -> e
             | None -> raise (Terminate "An environment should have been specified at this point")
            )
            in
            let arrayEnv = 
                (match arrayEnvOpt with
                 | Some e -> e
                 | None -> raise (Terminate "An environment should have been specified at this point")
                )
            in
            List.iter (fun (vName, intVal, additional) -> 
                let llVal = 
                match additional with 
                | None -> (build_alloca basellType vName bldr)
                | Some nItems -> 
                    let _ = Hashtbl.add arrayEnv vName true in
                    (*(build_alloca (array_type basellType intVal) vName bldr)*)
                    (build_array_alloca basellType nItems vName bldr)
                in
                Hashtbl.add env vName llVal
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
        let env:(string, llvalue) Hashtbl.t = Hashtbl.create (List.length decls) in
        let arrayEnv:(string, bool) Hashtbl.t = Hashtbl.create (List.length decls) in
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
        let parentFuncStr = (string_of_list parentFuncStrList) in
        let id = 
            if scope = SGlobal then name
            else "_" ^ parentFuncStr ^ name 
        in
        let _ = 
            (match (lookup_function id llm) with
             | Some f -> delete_function f (*NOTE: this whole thing is a stinking hack*)
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
        let decls = 
            if (llType = non_type) then decls
            else VarDecl(OType(bType, pointerCnt), [ADeclarator("_retVal", None)]) :: decls 
        in
        let funcDefList = List.fold_left (fun acc d -> 
                                    match d with
                                    | FunDef _ as fd -> fd :: acc
                                    | _ as d -> 
                                        let insBlock = insertion_block bldr in 
                                        generate_code d SInternal (Some env) (Some arrayEnv) newFuncStrList None bldr;
                                        position_at_end insBlock bldr;
                                        acc) [] decls in
        let triple = generate_triple env (string_of_list newFuncStrList) bldr in
        let _ = generate_func_declarations funcDefList (string_of_list newFuncStrList) triple (*bldr*) in
        let _ = List.iter (fun d ->
                                let insBlock = insertion_block bldr in
                                generate_code d SInternal (Some env) (Some arrayEnv) newFuncStrList (Some triple) bldr;
                                position_at_end insBlock bldr) (List.rev funcDefList) in
        (*let _ = List.iter (fun d -> codegen_stmt d env labels newFuncStrList bldr) stmts in*)
        let generatedReturn: bool ref = ref false in
        let basicBlocksInNeedOfABranchTarget = 
            List.fold_left (fun acc d -> 
                let stuff = 
                   (match d with
                    | SReturn _ ->
                        if !generatedReturn = false then begin
                            generatedReturn := true;
                            codegen_stmt d env arrayEnv labels newFuncStrList generatedReturn bldr
                        end
                        else
                            []
                    | SBlock _ ->
                        if !generatedReturn = false then
                            codegen_stmt d env arrayEnv labels newFuncStrList generatedReturn bldr
                        else
                            []
                    | _ ->
                        if !generatedReturn = false then begin
                            let newRef : bool ref = ref false in
                            codegen_stmt d env arrayEnv labels newFuncStrList newRef bldr
                        end
                        else
                            []
                   )
                in
                acc @ stuff
                ) [] stmts in
        if basicBlocksInNeedOfABranchTarget = [] then
            ignore (build_ret_void bldr)
        else begin
            let haveGeneratedReturnForFuncBlock: bool ref = ref false in
            if (List.length basicBlocksInNeedOfABranchTarget > 1) then
                let currentBB = insertion_block bldr in
                let parentFunction = block_parent currentBB in
                let returnBB = append_block llctx "func_return" parentFunction in
                position_at_end returnBB bldr;
                let retLLVal = locate_llval env "_retVal" parentFuncStrList bldr in
                let retLLVal = build_load retLLVal "ret_load" bldr in
                ignore (build_ret retLLVal bldr);

                List.iter (fun d ->
                    match d with
                    | ReturnBranch bb ->
                        position_at_end bb bldr;
                        ignore (build_br returnBB bldr)
                    | _ -> raise (Terminate "Only ReturnBranches should have been in this list")
                ) basicBlocksInNeedOfABranchTarget
            else
                let retLLVal = locate_llval env "_retVal" parentFuncStrList bldr in
                let retLLVal = build_load retLLVal "ret_load" bldr in
                ignore (build_ret retLLVal bldr);
        end
        
and codegen_stmt stmt env arrayEnv labels parentFuncStrList generatedReturn bldr = 
    match stmt with
    | SExpr expr -> ignore (codegen_expr expr env arrayEnv parentFuncStrList bldr); []
    | SBlock stmts -> (*ignore (List.hd (List.rev *)
        let aux acc d = 
            let bbOption = 
                (match d with
                 | SReturn _ as sr ->
                    if !generatedReturn = false then begin
                        generatedReturn := true;
                        codegen_stmt sr env arrayEnv labels parentFuncStrList generatedReturn bldr
                    end
                    else (
                        []
                    )
                 | SBlock ss as sb ->
                    if !generatedReturn = false then
                        codegen_stmt sb env arrayEnv labels parentFuncStrList generatedReturn bldr
                    else
                        []
                 | _ as s -> 
                    if !generatedReturn = false then
                        let newRef: bool ref = ref false in
                        codegen_stmt s env arrayEnv labels parentFuncStrList newRef bldr
                    else
                        []
                )    
            in
            (*let bbOption = codegen_stmt d env arrayEnv labels parentFuncStrList bldr in*)
            (match bbOption with
             | [] -> acc
             | [a] -> a :: acc
             | _ as l -> l @ acc
             (*| _ -> raise *)
            )
        in
        List.rev (List.fold_left aux [] stmts)
        (*List.map (fun d -> codegen_stmt d env labels parentFuncStrList (*loopAfterthoughtOpt loopMergeOpt*) bldr) stmts)*)
    | SIf (condExpr, stmt) ->
        (*let thenBB, _, mergeBB = codegen_conditional_expr condExpr false None None env bldr in*)
        let thenBB, mergeBB = codegen_conditional_expr condExpr None None env arrayEnv parentFuncStrList bldr in
        position_at_end thenBB bldr;

        let possibleBlocks = codegen_stmt stmt env arrayEnv labels parentFuncStrList generatedReturn bldr in
        let newThenBB = insertion_block bldr in
        position_at_end newThenBB bldr;
        let ourBB =  
            List.filter (fun d ->
                match d with
                | ContinueBranch (bb, _)
                | BreakBranch (bb, _)
                | ReturnBranch bb -> bb = newThenBB) possibleBlocks
        in
        if (ourBB = []) then
            ignore (build_br mergeBB bldr)
        else
            ()
        ;
        position_at_end mergeBB bldr;
        possibleBlocks
    | SIfElse (condExpr, stmt1, stmt2) -> 
        (*let thenBB, elseBBOpt, mergeBB = codegen_conditional_expr condExpr true None None env bldr in*)
        let thenBB,elseBB = codegen_conditional_expr condExpr None None env arrayEnv parentFuncStrList bldr in
        position_at_end thenBB bldr;

        (*ignore (codegen_stmt stmt1 env labels parentFuncStrList bldr);*)
        let truePossibleBlocks = codegen_stmt stmt1 env arrayEnv labels parentFuncStrList generatedReturn bldr in
        let newThenBB = insertion_block bldr in
        position_at_end newThenBB bldr;
        let trueBB =  
            List.filter (fun d ->
                match d with
                | ContinueBranch (bb, _)
                | BreakBranch (bb, _)
                | ReturnBranch bb -> bb = newThenBB) truePossibleBlocks
        in
        ()
        ;

        (*let newThenBB = insertion_block bldr in*)
        let parentFunction = block_parent newThenBB in
        position_at_end elseBB bldr;

        (*ignore (codegen_stmt stmt2 env labels parentFuncStrList loopAfterthoughtOpt loopMergeOpt bldr)*)
        let newRef: bool ref = ref false in
        let falsePossibleBlocks = codegen_stmt stmt2 env arrayEnv labels parentFuncStrList newRef bldr in
        let newElseBB = insertion_block bldr in
        let falseBB =
            List.filter (fun d ->
                match d with
                | ContinueBranch (bb, _)
                | BreakBranch (bb, _)
                | ReturnBranch bb -> bb = newElseBB) falsePossibleBlocks
        in
        let mergeBB = append_block llctx "merge" parentFunction in
        if (trueBB = []) then (
            position_at_end newThenBB bldr;
            ignore (build_br mergeBB bldr);
        )else
            ();
        
        if (falseBB = []) then (
            position_at_end newElseBB bldr;
            ignore(build_br mergeBB bldr);
        )else
            ();
        position_at_end mergeBB bldr;
        truePossibleBlocks @ falsePossibleBlocks
    | SFor (labelOption, initialization, condition, afterthought, stmt) -> (*this case now [15/9] seems a little toxic...*)
        (match initialization with
         | None -> ()
         | Some i -> ignore (codegen_expr i env arrayEnv parentFuncStrList bldr)
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
            let conditionLLVal = codegen_expr c env arrayEnv parentFuncStrList bldr in
            let res = 
            (match c with
             | EId _ -> build_load conditionLLVal "tmp_load" bldr
             | _ -> conditionLLVal
            ) in
            Some res
        ) in
        let bodyBB = insertion_block bldr in
        let thenBB = append_block llctx "loop_body" parentFunction in
        let afterthoughtBB = append_block llctx "loop_afterthought" parentFunction in
        let mergeBB = append_block llctx "loop_merge" parentFunction in
        position_at_end thenBB bldr;
        (match labelOption with
         | None -> ()
         | Some labelStrn ->
            let continueStrn = labelStrn ^ "Cont" in
            let breakStrn = labelStrn ^ "Break" in
            begin
                Hashtbl.add labels continueStrn afterthoughtBB;
                Hashtbl.add labels breakStrn mergeBB
            end
        );

        (*ignore (codegen_stmt stmt env labels parentFuncStrList (*(Some afterthoughtBB) (Some mergeBB)*) bldr);*)
        let possibleBlocks = codegen_stmt stmt env arrayEnv labels parentFuncStrList generatedReturn bldr in
        let newThenBB = insertion_block bldr in
        position_at_end afterthoughtBB bldr;

        (match afterthought with
         | None -> ()
         | Some a -> 
            ignore (codegen_expr a env arrayEnv parentFuncStrList bldr);
        );
        ignore (build_br loopBB bldr);

        position_at_end newThenBB bldr;
        let ourBB =  
            List.filter (fun d ->
                match d with
                | ContinueBranch (bb, _)
                | BreakBranch (bb, _)
                | ReturnBranch bb -> bb = newThenBB) possibleBlocks
        in
        if (ourBB = []) then
            ignore (build_br afterthoughtBB bldr)
        else
            ()
        ;
 
        (*ignore (build_br afterthoughtBB bldr);*)
        position_at_end loopBB bldr;
        (match conditionLLVal with
         | None -> ignore (build_br thenBB bldr)
         | Some c -> ignore (build_cond_br c thenBB mergeBB bldr);
        );
        position_at_end mergeBB bldr;

        let restOfBlocks = 
            List.fold_left (fun acc d ->
                match d with
                | BreakBranch (breakBB, labelOpt) ->
                    position_at_end breakBB bldr;
                    (match labelOpt with
                     | None -> ignore (build_br mergeBB bldr)
                     | Some str ->
                        let targetBB = Hashtbl.find labels (str ^ "Break") in
                        ignore (build_br targetBB bldr)
                    );
                    acc
                | ContinueBranch (continueBB, labelOpt) ->
                    position_at_end continueBB bldr;
                    (match labelOpt with
                     | None -> ignore (build_br afterthoughtBB bldr)
                     | Some str ->
                        let targetBB = Hashtbl.find labels (str ^ "Cont") in
                        ignore (build_br targetBB bldr)
                    );
                    acc
                | _ as br -> br :: acc
            ) [] possibleBlocks
        in
        let _ = position_at_end mergeBB bldr in
        restOfBlocks
    | SContinue labelOption ->
        let currentBlock = insertion_block bldr in
        [ContinueBranch (currentBlock, labelOption)]
    | SBreak labelOption ->
        let currentBlock = insertion_block bldr in
        [BreakBranch (currentBlock, labelOption)]
    | SReturn exprOption ->
        let _ = 
            (match exprOption with
             | None -> build_ret_void bldr
             | Some expr ->
                let retLLVal = locate_llval env "_retVal" parentFuncStrList bldr in
                (match expr with
                 | ENull ->
                    let currentBB = insertion_block bldr in
                    let func = block_parent currentBB in
                    let retType = return_type (element_type (type_of func)) in
                    let nullExpr = const_null retType in
                    build_store nullExpr retLLVal bldr
                 | _ as e ->
                    let llValExpr = codegen_expr expr env arrayEnv parentFuncStrList bldr in
                    let llValRet =
                        (match expr with
                         | EId _ 
                         | EArray _ 
                         | EUnary(UnaryDeref, _ ) -> build_load llValExpr "tmp_load" bldr
                         | _ -> llValExpr
                        )
                    in
                    build_store llValRet retLLVal bldr
                )
            )
        in
        let currentBlock = insertion_block bldr in
        [ReturnBranch currentBlock]
    | _ -> []

and codegen_expr expr env arrayEnv parentFuncStrList bldr =  
    match expr with
    | EId name -> locate_llval env name parentFuncStrList bldr
    | EExpr nExpr -> codegen_expr nExpr env arrayEnv parentFuncStrList bldr
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
                    Array.of_list (process_args eList subArray env arrayEnv parentFuncStrList bldr)
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
        let exprLLV = codegen_expr aExpr env arrayEnv parentFuncStrList bldr in
        let offset = 
            let partial = codegen_expr idxExpr env arrayEnv parentFuncStrList bldr in
            (match idxExpr with
             | EId _ | EArray _ -> build_load partial "tmp_load" bldr
             | _ -> partial
            ) 
        in
        (match (classify_value exprLLV) with
         | Llvm.ValueKind.GlobalVariable -> 
            let constZero = const_int int_type 0 in
            let llv = build_in_bounds_gep exprLLV [|constZero; offset|] "tmp_global_access" bldr in
            llv
         | _ -> 
            let baseVal = 
                (match aExpr with
                 | EId id -> 
                    (try
                        let _ = Hashtbl.find arrayEnv id in
                        exprLLV
                    with
                    | Not_found -> build_load exprLLV "tmp_base_load" bldr
                    )
                 | _ -> exprLLV 
                )
            in
            build_in_bounds_gep baseVal [|offset|] "tmp_access" bldr
        )
    | EUnary (unaryOp, uExpr) ->
        let exprLLVal = codegen_expr uExpr env arrayEnv parentFuncStrList bldr in
        let exprValType = type_of exprLLVal in
        let constZero = const_int int_type 0 in
        (match unaryOp with
         | UnaryRef -> exprLLVal (*all llVals which could be referenced are stored as pointers already...*)
                 (*build_gep exprLLVal [|constZero; constZero|] "tmp_ref" bldr*)
         | UnaryDeref ->
             (*let ptrLLVal = build_gep exprLLVal [|constZero; constZero|] "tmp_ref" bldr in*)
             (match uExpr with
             | EFCall _ -> exprLLVal
             | _ ->
                let ptrLLVal = build_load exprLLVal "tmp_ptr" bldr in
                ptrLLVal
             )
             (*build_load ptrLLVal "tmp_load" bldr*)
         | UnaryPlus -> exprLLVal
         | UnaryMinus -> 
            let llVal = 
                (match uExpr with
                 | EId _
                 | EArray _ -> build_load exprLLVal "tmp_load" bldr
                 | _ -> exprLLVal
            )
            in
            if ((type_of llVal) = int_type) then
                build_neg llVal "tmp_neg" bldr
            else
                build_fneg llVal "tmp_fneg" bldr
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
            let trueBB, falseBB = codegen_conditional_expr binExpr None None env arrayEnv parentFuncStrList bldr in
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
            let trueBB, falseBB = codegen_conditional_expr binExpr None None env arrayEnv parentFuncStrList bldr in
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
            let llVal1 = codegen_expr opand1 env arrayEnv parentFuncStrList bldr in
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
            let llVal2 = codegen_expr opand2 env arrayEnv parentFuncStrList bldr in
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
        let llValExpr = codegen_expr expr env arrayEnv parentFuncStrList bldr in
        let llValExprLoad = build_load llValExpr "tmp_load" bldr in
        let valType = type_of llValExprLoad in
        let oneConst = 
            if valType = int_type then 
                const_int int_type 1 
            else
                const_float double_type 1.0
        in
        let modifiedLLValExpr = 
        (match unAssOp with
         | UnaryPlusPlus -> 
            if valType = int_type then 
                build_add llValExprLoad oneConst "tmp_inc" bldr
            else
                build_fadd llValExprLoad oneConst "tmp_inc" bldr
         | UnaryMinusMinus -> 
            if valType = int_type then 
                build_sub llValExprLoad oneConst "tmp_inc" bldr
            else
                build_fsub llValExprLoad oneConst "tmp_inc" bldr
        ) in
        let res = build_store modifiedLLValExpr llValExpr bldr in
        (match opLocation with
         | LocRight -> llValExprLoad 
         | LocLeft -> modifiedLLValExpr
        )
    | EBinAssign (binAssOp, expr1, expr2) -> 
        let leftHandLLValOpt, rightHandLLVal = 
            (match binAssOp with
            | BinAssign -> 
                let llVal1 = codegen_expr expr1 env arrayEnv parentFuncStrList bldr in
                let llVal2 = codegen_expr expr2 env arrayEnv parentFuncStrList bldr in
                (match expr2 with
                 | ENull -> Some llVal1, const_null (element_type (type_of llVal1))
                 | EId _
                 | EArray _ 
                 | EUnary(UnaryDeref, _) -> Some llVal1, build_load llVal2 "tmp_load" bldr
                 | _ -> Some llVal1, llVal2
                )
            | BinAssignMulti -> None, codegen_expr (EBinOp (BinMulti, expr1, expr2)) env arrayEnv parentFuncStrList bldr
            | BinAssignDiv -> None, codegen_expr (EBinOp (BinDiv, expr1, expr2)) env arrayEnv parentFuncStrList bldr
            | BinAssignMod -> None, codegen_expr (EBinOp (BinMod, expr1, expr2)) env arrayEnv parentFuncStrList bldr
            | BinAssignPlus -> None, codegen_expr (EBinOp (BinPlus, expr1, expr2)) env arrayEnv parentFuncStrList bldr
            | BinAssignMinus -> None, codegen_expr (EBinOp (BinMinus, expr1, expr2)) env arrayEnv parentFuncStrList bldr
            ) 
        in
        let llVal1 = 
            (match leftHandLLValOpt with
             | Some l -> l
             | None ->
                let llVal1 = codegen_expr expr1 env arrayEnv parentFuncStrList bldr in
                if (pointer_type (type_of rightHandLLVal) = type_of llVal1) then llVal1
                else build_load llVal1 "tmp_load" bldr
            )
        in
        let _ = build_store rightHandLLVal llVal1 bldr in
        rightHandLLVal
    | EConditional (exprCondition, exprTrue, exprFalse) -> 
        let trueBB, falseBB = codegen_conditional_expr exprCondition None None env arrayEnv parentFuncStrList bldr in
        let parentFunc = block_parent trueBB in
        let mergeBB = append_block llctx "merge" parentFunc in
        position_at_end trueBB bldr;

        let llVal1 = codegen_expr exprTrue env arrayEnv parentFuncStrList bldr in
        let llVal1 =
        (match exprTrue with
         | EId _ 
         | EArray _ -> build_load llVal1 "tmp_load" bldr
         | _ -> llVal1
        )
        in
        ignore (build_br mergeBB bldr);

        position_at_end falseBB bldr;
        let llVal2 = codegen_expr exprFalse env arrayEnv parentFuncStrList bldr in
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
         | Some (ArrExp exp) -> codegen_expr exp env arrayEnv parentFuncStrList bldr
        ) in
        let sizeOf = 
            let sizeConst = 
                if pointerCnt = 1 then
                    (match basicType with
                    | TInt -> 2
                    | TChar -> 1
                    | TBool -> 1
                    | TDouble -> 10
                    | _ -> raise (Terminate "shit\n")
                    )
                else
                    8
            in
            const_int int_type sizeConst
        in
        let sizeLLVal = build_mul sizeOf countLLVal "tmp_size" bldr in
        let res = 
            (match lookup_function "Allocate" llm with
             | None -> 
                let allocateType = function_type (pointer_type (i8_type llctx)) [|int_type|] in
                let allocateFunc = declare_function "Allocate" allocateType llm in
                build_call allocateFunc [|sizeLLVal|] "tmp_new" bldr
             | Some funLLValue ->
                build_call funLLValue [|sizeLLVal|] "tmp_new" bldr
            )
        in
        build_bitcast res llValType "tmp_allocate_res" bldr
    | ENewP (OType(bType, pointerCnt), newExprOption) ->
        let llValType = get_llvm_type bType pointerCnt in
        let countLLVal = 
        (match newExprOption with
        | Some (Some (ArrExp exp), None) -> codegen_expr exp env arrayEnv parentFuncStrList bldr
        | None -> const_int int_type 1
        | _ -> raise (Terminate "This shouldn't have happened")
        ) in
        let sizeOf = const_int int_type 8 in
        let sizeLLVal = build_mul sizeOf countLLVal "tmp_size" bldr in
        let res = 
            (match lookup_function "Allocate" llm with
             | None -> 
                let allocateType = function_type (pointer_type (i8_type llctx)) [|int_type|] in
                let allocateFunc = declare_function "Allocate" allocateType llm in
                build_call allocateFunc [|sizeLLVal|] "tmp_new" bldr
             | Some funLLValue ->
                build_call funLLValue [|sizeLLVal|] "tmp_new" bldr
            )
        in
        build_bitcast res llValType "tmp_allocate_res" bldr
    | EDelete exp -> 
        let llValExpr = codegen_expr exp env arrayEnv parentFuncStrList bldr in
        let voidPtrType = pointer_type (i8_type llctx) in
        let pointerToDestroy = build_load llValExpr "tmp_load" bldr in
        let pointerToDestroy = build_bitcast pointerToDestroy voidPtrType "tmp_dealloc_res" bldr in
        (match lookup_function "Deallocate" llm with
         | None ->
                 let deallocateType = function_type voidPtrType [|voidPtrType|] in
                 let deallocateFunc = declare_function "Deallocate" deallocateType llm in
                 build_call deallocateFunc [|pointerToDestroy|] "tmp_dealloc" bldr
         | Some funLLValue ->
             build_call funLLValue [|pointerToDestroy|] "tmp_dealloc" bldr
        )
    | ECast (OType (bType, pointerCnt), expr) ->
        let llTargetType = get_llvm_type bType pointerCnt in
        let exprLLVal = codegen_expr expr env arrayEnv parentFuncStrList bldr in
        let exprLLVal = 
            (match expr with
                | EId _ | EArray _ -> build_load exprLLVal "tmp_load" bldr
                | _ -> exprLLVal
            )
        in
        let llSourceType = type_of exprLLVal in
        let convResult = 
            if llTargetType = int_type then (
                if llSourceType = double_type then
                    build_fptosi exprLLVal int_type "tmp_fptosi" bldr
                else if llSourceType = char_type then
                    build_zext exprLLVal int_type "tmp_chartosi" bldr
                else if llSourceType = bool_type then
                    build_zext exprLLVal int_type "tmp_booltosi" bldr
                else 
                    exprLLVal
            )
            else if llTargetType = double_type then (
                if llSourceType = int_type then (
                    build_sitofp exprLLVal double_type "tmp_sitofp" bldr
                )
                else if llSourceType = char_type then
                    build_uitofp exprLLVal double_type "tmp_chartofp" bldr
                else if llSourceType = bool_type then
                    build_uitofp exprLLVal double_type "tmp_booltofp" bldr
                else 
                    exprLLVal
            )
            else if llTargetType = char_type then (
                if llSourceType = int_type then
                    build_trunc exprLLVal char_type "tmp_sitochar" bldr 
                else if llSourceType = double_type then
                    build_fptoui exprLLVal char_type "tmp_fptochar" bldr
                else if llSourceType = bool_type then
                    build_zext exprLLVal char_type "tmp_booltochar" bldr
                else 
                    exprLLVal
            )
            else if llTargetType = bool_type then (
                if llSourceType = double_type then
                    let constZero = const_float double_type 0.0 in 
                    build_fcmp Fcmp.One exprLLVal constZero "tmp_fptobool" bldr
                else if llSourceType = int_type then
                    let constZero = const_int int_type 0 in 
                    build_icmp Icmp.Ne exprLLVal constZero "tmp_itobool" bldr
                else if llSourceType = char_type then
                    let constZero = const_int char_type 0 in
                    build_icmp Icmp.Ne exprLLVal constZero "tmp_ctobool" bldr
                else 
                    exprLLVal
            )
            else (*this will match casts between pointer types*) (
                let bitcastNeeded = (llTargetType <> llSourceType) in (*this comparison might not work...*)
                let actualLLVal =
                (match expr with
                 | EId _ 
                 | EUnary(UnaryDeref, _) -> exprLLVal (*build_load exprLLVal "tmp_load" bldr*)
                 | EUnary(UnaryRef, _) -> exprLLVal (*should do nothing, since the exprLLVal is the result of getelementpointer?*)
                 | _ -> exprLLVal
                ) in
                if bitcastNeeded then 
                    build_bitcast actualLLVal llTargetType "tmp_ptrtoptr" bldr
                else
                    actualLLVal
            )
        in
        convResult

and locate_llval env name parentFuncStrList bldr = 
    try
        Hashtbl.find env name
    with
    | Not_found -> 
        (try
            let v = Hashtbl.find env ("_ref" ^ name) in
            build_load v "tmp_load" bldr
        with
        | Not_found ->
            let rec aux l = 
                let vName, lNames = 
                (match l with
                 | [] -> name, []
                 | _ as lN -> "_" ^ (string_of_list lN) ^ "_ref" ^ name, lN
                )
                in
                try
                    let v = Hashtbl.find env vName in
                    build_load v "tmp_load" bldr
                with
                | Not_found ->
                    if (l = []) then begin
                        let resOption = lookup_global name llm in
                        match resOption with
                        | Some x -> x
                        | None -> 
                            Printf.printf "\x1b[31mError\x1b[0m: Couldn't locate %s during generation.\n" name;
                            dump_module llm;
                            exit 1
                    end
                    else aux (List.tl l)
            in
            aux parentFuncStrList
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

and process_args eList paramTypeArray env arrayEnv parentFuncStrList bldr = 
    let paramTypeList = Array.to_list paramTypeArray in
    let aux e p = 
        let eLLVal = codegen_expr e env arrayEnv parentFuncStrList bldr in
        let eType = type_of eLLVal in
        if (eType = p) then eLLVal
        else begin
            let eLLVal = build_load eLLVal "tmp_load" bldr in
            if (p = (type_of eLLVal)) then eLLVal
            else build_load eLLVal "tmp_load" bldr
        end
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
                    ("_ref" ^ xName), get_llvm_type bType (pointerCnt + 1)
        )
        in
        let llTypedParams = List.map aux paramList in
        let names = List.map (fun (a, _) -> a) llTypedParams in
        let types = List.map (fun (_, b) -> b) llTypedParams in
        Array.of_list names, Array.of_list types

and codegen_conditional_expr condExpr trueBBOption falseBBOption env arrayEnv parentFuncStrList bldr = (*NOTE: caller is responsible for adding 
                                                                                                        *      branches at the end of basic blocks *)
    let aux e trueBBOption falseBBOption =
        let exprLLVal = codegen_expr e env arrayEnv parentFuncStrList bldr in
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
                    codegen_conditional_expr e trueBBOption falseBBOption env arrayEnv parentFuncStrList bldr
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
                        codegen_conditional_expr e trueBBOption (Some falseBB1) env arrayEnv parentFuncStrList bldr
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
                        codegen_conditional_expr e (Some trueBB1) falseBBOption env arrayEnv parentFuncStrList bldr
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
        if (k = "_retVal") then
            ()
        else if ((String.length k) > 4 && (String.sub k 0 4 = "_ref" || (String.sub k 0 4 <> "_ref" && k.[0] = '_'))) then (*NOTE: this check looks like shit, will probably perform like shit*)
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
            let _ = List.iter (fun x -> generate_code x SGlobal None None [""] None bldr) tree in
            llm
        with
        | _ as e -> Printf.printf "\x1b[31mException\x1b[0m: %s\n" (Printexc.to_string e); dump_module llm; exit 1
