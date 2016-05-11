(*NOTE (achilles): from https://ocaml.org/learn/tutorials/format.html#Differencesbetweenapackingandastructuralhovbox:
    * Do not try to force new lines, let the pretty-printer do it for you: that's its only job. In particular, 
    * do not use force_newline: this procedure effectively leads to a newline, but it also as the unfortunate 
    * side effect to partially reinitialise the pretty-printing engine, so that the rest of the printing material 
    * is noticeably messed up.
    
Even following this advice, pretty printing is shitty printing... maybe write our own pretty printer?*)

open Format

type ast_program = ast_decl list

and ast_decl = VarDecl of obj_type * ast_declarator list
             | FunDecl of obj_type * string * (ast_param list) option
             | FunDef of obj_type * string * (ast_param list) option * ast_decl list * ast_stmt list

(* NOTE (achilles): the int is supposed to represent the number of '*', but maybe change 
         it because it seems like a bad idea? *)
and ast_declarator = ADeclarator of string * ast_expr option

and obj_type = OType of basic_type * int 

and basic_type = TVoid
               | TInt
               | TChar
               | TBool
               | TDouble

and ast_param = Param of obj_type * string
              | ParamByRef of obj_type * string

and ast_stmt = SSemicolon
             | SExpr of ast_expr
             | SBlock of ast_stmt list
             | SIf of ast_expr * ast_stmt
             | SIfElse of ast_expr * ast_stmt * ast_stmt
             | SFor of (string option) * (ast_expr option) * (ast_expr option) * (ast_expr option) * ast_stmt
             | SContinue of (string option)
             | SBreak of (string option)
             | SReturn of (ast_expr option)

and ast_expr = EId of string
             | EExpr of ast_expr
             | EBool of bool
             | EInt of int
             | EChar of char
             | EDouble of float
             | EString of string
             | ENull
             | EFCall of string * (ast_expr list option)
             | EArray of ast_expr * ast_array_exp
             | EUnary of ast_un * ast_expr
             | EBinOp of ast_bin_op * ast_expr * ast_expr
             | EUnAssign of ast_un_ass * ast_ass_loc * ast_expr
             | EBinAssign of ast_bin_ass * ast_expr * ast_expr
             | ECast of obj_type * ast_expr
             | EConditional of ast_expr * ast_expr * ast_expr
             | ENew of obj_type * (ast_array_exp option)
             | ENewP of ast_expr * int * ((ast_array_exp option * ast_expr option) option)
             | EDelete of ast_expr
and ast_array_exp = ArrExp of ast_expr

and ast_un = UnaryRef | UnaryDeref | UnaryPlus | UnaryMinus | UnaryNot

and ast_bin_ass = BinAssign
                | BinAssignMulti
                | BinAssignDiv
                | BinAssignMod
                | BinAssignPlus
                | BinAssignMinus

and ast_bin_op = BinDiv 
               | BinMulti 
               | BinMod 
               | BinPlus 
               | BinMinus 
               | BinLess 
               | BinGreater 
               | BinLessEq
               | BinGreaterEq
               | BinEq
               | BinNotEq
               | BinAnd
               | BinOr
               | BinComma

and ast_un_ass = UnaryPlusPlus
               | UnaryMinusMinus

and ast_ass_loc = LocLeft
                | LocRight

let astTree : ast_decl list ref = ref []

let rec print_ast_program ppf ast =
  match ast with
  | []    -> 
    ()
  | [h] -> 
    print_ast_declaration ppf h
  | h :: t  -> 
    print_ast_declaration ppf h;
    print_newline ();
    print_newline ();
    print_ast_program ppf t

and print_ast_declaration ppf ast = 
    match ast with
    | VarDecl(OType (bType, pointerCnt), declList) ->
      let typeString =  ast_type_string bType in
      fprintf ppf "%s" typeString;
      ast_print_pointers ppf pointerCnt;
      print_ast_declarators ppf declList
    | FunDecl(OType(bType, pointerCnt), name, paramOption) ->
      let typeString = ast_type_string bType in
      fprintf ppf "%s" typeString;
      ast_print_pointers ppf pointerCnt;
      fprintf ppf " %s(" name;
      let paramList = (match paramOption with
      | None -> []
      | Some x -> x
      )
      in
      print_ast_params ppf paramList;
      fprintf ppf ")"
    | FunDef(OType(bType, pointerCnt), name, paramOption, decls, stmts) ->
      let typeString = ast_type_string bType in
      fprintf ppf "%s" typeString;
      ast_print_pointers ppf pointerCnt;
      fprintf ppf " %s(" name;
      let paramList = (match paramOption with
      | None -> ()
      | Some x -> print_ast_params ppf x
      )
      in ignore paramList
      ;
      fprintf ppf "){";
      open_vbox 2;
      print_cut ();
      (match decls with
       | [] -> ()
       | _ -> print_ast_declarations ppf decls 
      )
      ;
      print_ast_stmts ppf stmts;
      close_box ();
      print_cut ();
      fprintf ppf "}"

and print_ast_declarations ppf decls = 
    match decls with
    | [] -> ()
    | [h] -> print_ast_declaration ppf h
    | h :: t ->
        print_ast_declaration ppf h;
        print_cut ();
        print_ast_declarations ppf t

and print_ast_declarators ppf decls = 
    match decls with
    | [] -> ()
    | [decl] -> print_ast_declarator ppf decl
    | decl :: tl -> 
        print_ast_declarator ppf decl; 
        fprintf ppf ", "; 
        print_ast_declarators ppf tl

and print_ast_declarator ppf decl = 
    match decl with
    | ADeclarator(name, None) -> fprintf ppf " %s" name
    | ADeclarator(name, Some x) -> fprintf ppf "%s[" name; print_ast_expr ppf x; fprintf ppf "]"

and ast_type_string bType = 
    match bType with
    | TVoid -> "void"
    | TInt -> "int"
    | TChar -> "char"
    | TBool -> "bool"
    | TDouble -> "double"

and ast_print_pointers ppf pointerCnt =
    for i = 1 to pointerCnt do
        fprintf ppf "*" 
    done 

and print_ast_params ppf params =
  match params with
  | [] ->
    ()
  | [param] ->
    print_ast_param ppf param
  | param::rest ->
    print_ast_param ppf param;
    fprintf ppf ", ";
    print_ast_params ppf rest

and print_ast_param ppf ast =
  match ast with
  | Param (OType(bType, pointerCnt), name) ->
    let typeString = ast_type_string bType in
    fprintf ppf "%s" typeString;
    ast_print_pointers ppf pointerCnt;
    fprintf ppf " %s" name;
  | ParamByRef (OType(bType, pointerCnt), name) ->
    let typeString = ast_type_string bType in
    fprintf ppf "byref %s" typeString;
    ast_print_pointers ppf pointerCnt;
    fprintf ppf " %s" name;

and print_ast_stmts ppf stmts =
  match stmts with
  | [] ->
    ()
  | [stmt] ->
    print_ast_stmt ppf stmt
  | stmt::rest ->
    print_ast_stmt ppf stmt;
    print_cut ();
    print_ast_stmts ppf rest

and print_ast_stmt ppf stmt =
  match stmt with
  | SSemicolon ->
    fprintf ppf ";"
  | SExpr expr->
    print_ast_expr ppf expr
  | SBlock stmts ->
    fprintf ppf "{";
    open_vbox 2;
    print_cut ();
    print_ast_stmts ppf stmts;
    close_box ();
    print_cut ();
    fprintf ppf "}";
  | SIf (expr, stmt) ->
    fprintf ppf "if (";
    print_ast_expr ppf expr;
    fprintf ppf ") then ";
    print_ast_stmt ppf stmt; 
  | SIfElse (expr, stmt, elseStmt) ->
    fprintf ppf "if (";
    print_ast_expr ppf expr;
    fprintf ppf ") then ";
    print_ast_stmt ppf stmt; 
    fprintf ppf " else ";
    print_ast_stmt ppf elseStmt;
  | SFor ( label, initialization, condition, afterthought, stmt) ->
    (match label with
    | None -> ()
    | Some l -> fprintf ppf "%s: " l
    );
    fprintf ppf "for (";
    (match initialization with
    | None -> ()
    | Some i -> print_ast_expr ppf i
    );
    fprintf ppf "; ";
    (match condition with
    | None -> ()
    | Some c -> print_ast_expr ppf c
    );
    fprintf ppf "; ";
    (match afterthought with
    | None -> ()
    | Some a -> print_ast_expr ppf a
    );
    fprintf ppf ")";
    open_vbox 2;
    print_cut ();
    print_ast_stmt ppf stmt;
    close_box ();
    print_cut ();
  | SContinue label ->
    fprintf ppf "continue";
    (match label with
    | None -> ()
    | Some l -> fprintf ppf " %s" l
    );
  | SBreak label ->
    fprintf ppf "break";
    (match label with
    | None -> ()
    | Some l -> fprintf ppf " %s" l
    );
  | SReturn expr ->
    fprintf ppf "return";
    (match expr with
    | None -> ()
    | Some e -> fprintf ppf " "; print_ast_expr ppf e
    );

and print_ast_expr ppf ast =
  match ast with
  | EId name ->
    fprintf ppf "%s" name
  | EExpr exp ->
    print_ast_expr ppf exp
  | EBool bVal ->
    (match bVal with
    | true -> fprintf ppf "true"
    | false -> fprintf ppf "false"
    )
  | EInt iVal ->
    fprintf ppf "%d" iVal
  | EChar cVal ->
    fprintf ppf "%c" cVal
  | EDouble dVal ->
    fprintf ppf "%f" dVal
  | EString str ->
    fprintf ppf "%S" str
  | ENull ->
    fprintf ppf "null"
  | EFCall (fName, actualParamsOption) ->
    fprintf ppf "%s (" fName;
    (match actualParamsOption with
     | None -> ()
     | Some actualParams -> print_ast_actual_params ppf actualParams;
    );
    fprintf ppf ")"
  | EArray (expr, arrExpr) ->
    print_ast_expr ppf expr;
    print_ast_arr_expr ppf arrExpr
  | EUnary (unaryOp, expr) ->
    (match unaryOp with
    | UnaryRef -> fprintf ppf "&"
    | UnaryDeref -> fprintf ppf "*"
    | UnaryPlus -> fprintf ppf "+"
    | UnaryMinus -> fprintf ppf "-"
    | UnaryNot -> fprintf ppf "!"
    );
    print_ast_expr ppf expr
  | EBinOp (binaryOp, expr1, expr2)  ->
    print_ast_expr ppf expr1;
    let printfun = fprintf ppf in 
    (match binaryOp with
    | BinDiv -> printfun "/"
    | BinMulti -> printfun "*"
    | BinMod -> printfun "%%"
    | BinPlus -> printfun "+"
    | BinMinus -> printfun "-"
    | BinLess -> printfun "<"
    | BinGreater -> printfun ">"
    | BinLessEq -> printfun "<="
    | BinGreaterEq -> printfun ">="
    | BinEq -> printfun "=="
    | BinNotEq -> printfun "!="
    | BinAnd -> printfun "&&"
    | BinOr -> printfun "||"
    | BinComma -> printfun ","
    );
    print_ast_expr ppf expr2;
  | EUnAssign (unaryAss, assLocation, expr) ->
    (match assLocation with
    | LocLeft -> ast_print_un_ass ppf unaryAss; print_ast_expr ppf expr
    | LocRight -> print_ast_expr ppf expr; ast_print_un_ass ppf unaryAss
    )
  | EBinAssign (binaryAss, expr1, expr2) ->
    print_ast_expr ppf expr1;
    let printfun = fprintf ppf in 
    (match binaryAss with
    | BinAssign  -> printfun "="
    | BinAssignMulti -> printfun "*="
    | BinAssignDiv -> printfun "/="
    | BinAssignMod -> printfun "%%="
    | BinAssignPlus -> printfun "+="
    | BinAssignMinus -> printfun "-="
    );
    print_ast_expr ppf expr2;
  | ECast (OType(bType, pointerCnt), expr) ->
    fprintf ppf "( ";
    fprintf ppf "%s" (ast_type_string bType);
    ast_print_pointers ppf pointerCnt;
    fprintf ppf ") ";
    print_ast_expr ppf expr;
  | EConditional (conditionExpr, trueExpr, falseExpr) ->
    print_ast_expr ppf conditionExpr;
    fprintf ppf " ? ";
    print_ast_expr ppf trueExpr;
    fprintf ppf " : [";
    print_ast_expr ppf falseExpr;
    fprintf ppf "] "
  | ENew (OType(bType, _), arrOption) ->
    fprintf ppf "new %s" (ast_type_string bType);
    (*ast_print_pointers pointerCnt;*)
    (match arrOption with
     | None -> ()
     | Some x -> print_ast_arr_expr ppf x
    )
  | ENewP (enew, pointerCnt, exprOption) ->
    print_ast_expr ppf enew;
    ast_print_pointers ppf pointerCnt;
    (
        match exprOption with
        | Some (Some x, None) -> print_ast_arr_expr ppf x
        | Some (None, Some x) -> print_ast_expr ppf x 
        | None | Some (_, _) -> () (*something went horribly wrong*)
    
    );
  | EDelete expr ->
    fprintf ppf "delete ";
    print_ast_expr ppf expr

and print_ast_arr_expr ppf arrExpr =
    fprintf ppf "[";
    let ArrExp expr = arrExpr in
    print_ast_expr ppf expr;
    fprintf ppf "]"

and ast_print_un_ass ppf ass = 
    match ass with
    | UnaryPlusPlus -> fprintf ppf "++"
    | UnaryMinusMinus -> fprintf ppf "--"

and print_ast_actual_params ppf exprs =
  match exprs with
  | [] -> ()
  | [expr] ->
    print_ast_expr ppf expr
  | expr::rest ->
    print_ast_expr ppf expr;
    fprintf ppf ", ";
    print_ast_actual_params ppf rest

and pretty_print ppf ast =
   match ast with
   | [] ->
      eprintf "%s@." "AST is empty"
   | tree ->
      print_ast_program ppf tree

let print_ast ast_tree = 
  force_newline ();
  printf "*** \"Pretty\" Printing AST ***";
  force_newline ();
  printf "***************************";
  force_newline ();
  printf "%a" pretty_print ast_tree;
  print_newline ()
