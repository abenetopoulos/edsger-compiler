%{
    open Ast
%}

%token <int> INT
%token <float> DOUBLE
%token <string> ID
%token <char> CHAR
%token <string> STRING
%token INT_T
%token CHAR_T
%token BOOL_T
%token DOUBLE_T
%token SEMICOLON
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token VOID
%token BYREF
%token LEFT_CURL
%token RIGHT_CURL
%token COLON
%token IF
%token ELSE
%token FOR
%token CONTINUE
%token BREAK
%token RETURN
%token TRUE
%token FALSE
%token NULL
%token QUESTION_MARK
%token NEW
%token DELETE
%token NOT
%token MULTI
%token DIV
%token MOD
%token PLUS
%token MINUS
%token AMBER
%token LESS
%token GREATER
%token LESSEQ
%token GREATEREQ
%token EQ
%token NOTEQ
%token AND
%token OR
%token COMMA
%token PLUSPLUS
%token MINUSMINUS
%token ASSIGN
%token ASSIGN_MULTI
%token ASSIGN_DIV
%token ASSIGN_MOD
%token ASSIGN_PLUS
%token ASSIGN_MINUS
%token EOF

%start <unit> prog

%left COMMA
%right ASSIGN ASSIGN_MULTI ASSIGN_DIV ASSIGN_MOD ASSIGN_PLUS ASSIGN_MINUS
%right QUEST
%nonassoc QUESTION_MARK
%left BINARY
%left OR
%left AND
%nonassoc EQ NOTEQ GREATEREQ GREATER LESSEQ LESS
%left PLUS MINUS
%left MULTI DIV MOD
%nonassoc LEFT_PAREN
%nonassoc PLUSPLUS MINUSMINUS
%nonassoc NEW DELETE
%nonassoc AMBER
%nonassoc PLUSPLUS_PRE
%nonassoc LEFT_BRACKET
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%

prog:
    | declaration; declaration*; EOF                                       { astTree := !astTree @ ($1 :: $2) }
    ;

declaration:
    | var_declaration                                                      { $1 }
    | fun_declaration                                                      { $1 }
    | fun_definition                                                       { $1 }
    ;

var_declaration:
    | object_type; declarator; other_decs*; SEMICOLON                      { VarDecl ($1, $2 :: $3) }
    ;

other_decs:
    | COMMA; declarator;                                                   { $2 }

object_type:
    | basic_type; MULTI*                                                   { OType ($1, List.length $2) }
    ;

basic_type:
    | INT_T                                                                { TInt }
    | CHAR_T                                                               { TChar }
    | BOOL_T                                                               { TBool }
    | DOUBLE_T                                                             { TDouble }
    ;

declarator:
    | ID; array_def?                                                       { ADeclarator ($1, $2) }
    ;

array_def:
    | LEFT_BRACKET; const_expr; RIGHT_BRACKET                              { $2 }
    ;

fun_declaration:
    | VOID ; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; SEMICOLON       {  let oType = OType(TVoid, 0) in 
                                                                             FunDecl (oType, $2, $4)  }
    | object_type; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; SEMICOLON { let oType = $1 in
                                                                             FunDecl (oType, $2, $4)}
    ;

parameter_list:
    | parameter; other_params*                                             { $1 :: $2 }
    ;

other_params:
    | COMMA; parameter                                                     { $2 }
    ;

parameter:
    | BYREF?; object_type; ID                                              { let v = 
                                                                                match $1 with
                                                                                | None -> Param ($2, $3)
                                                                                | _ -> ParamByRef ($2, $3)
                                                                             in v 
                                                                           }
    ;

fun_definition:
    | VOID; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; LEFT_CURL; 
      declaration*; statement*; RIGHT_CURL                                 { let oType = OType(TVoid, 0) in 
                                                                             FunDef (oType, $2, $4, $7, $8) }
    | object_type; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; LEFT_CURL; 
      declaration*; statement*; RIGHT_CURL                                 { let oType = $1 in
                                                                             FunDef (oType, $2, $4, $7, $8)}
    ;

statement:
    | SEMICOLON                                                            { SSemicolon }
    | expression; SEMICOLON                                                { SExpr $1 }
    | LEFT_CURL; statement*; RIGHT_CURL                                    { SBlock $2 } (*NOTE: should we use fold here?*)
    | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement                   { SIf ($3, $5) }     %prec LOWER_THAN_ELSE
    | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement; ELSE; statement  { SIfElse ($3, $5, $7) }
    | label?; FOR; LEFT_PAREN; expression?; SEMICOLON; expression?; 
      SEMICOLON; expression?; RIGHT_PAREN; statement                       { SFor ($1, $4, $6, $8, $10) }
    | CONTINUE; ID?; SEMICOLON                                             { SContinue $2 }
    | BREAK; ID?; SEMICOLON                                                { SBreak $2 }
    | RETURN; expression?; SEMICOLON                                       { SReturn $2 }
    ;

label:
    | ID; COLON                                                            { $1 }
    ;

expression:
    | ID                                                                   { EId $1 }
    | LEFT_PAREN; expression; RIGHT_PAREN                                  { $2 }
    | TRUE                                                                 { EBool true }
    | FALSE                                                                { EBool false }
    | NULL                                                                 { ENull }
    | INT                                                                  { EInt $1 }
    | CHAR                                                                 { EChar $1 }
    | DOUBLE                                                               { EDouble $1 }
    | STRING                                                               { EString $1 }
    | ID; LEFT_PAREN; expression_list?; RIGHT_PAREN                        { EFCall ($1, $3) }
    | expression; array_exp                                                { EArray ($1, $2) } 
    | unary_op; expression                                                 { EUnary ($1, $2) }     %prec AMBER
    | expression; binary_op; expression                                    { EBinOp ($2, $1, $3) }     %prec BINARY
    | unary_assign; expression                                             { EUnAssign ($1, LocLeft, $2) }     %prec PLUSPLUS_PRE
    | expression; unary_assign                                             { EUnAssign ($2, LocRight, $1) }
    | expression; binary_assign; expression                                { EBinAssign ($2, $1, $3) }     %prec ASSIGN_MINUS
    | LEFT_PAREN; object_type; RIGHT_PAREN; expression                     { ECast ($2, $4) }     %prec LEFT_PAREN
    | expression; QUESTION_MARK; expression; COLON; expression             { EConditional ($1, $3, $5) }     %prec QUEST
    | new_expression                                                       { $1 }
    | DELETE; expression                                                   { EDelete $2 }
    ;

new_expression:
    | NEW; basic_type; array_exp?                                          { ENew (OType($2, 0), $3) }
    | new_expression; MULTI+; opt_exp?                                     { ENewP ($1, List.length $2, $3) }
    ;

opt_exp:
    | array_exp                                                            { (Some $1, None) }
    | expression                                                           { (None, Some $1) }

array_exp:
    | LEFT_BRACKET; expression; RIGHT_BRACKET                              { ArrExp $2 }
    ;

expression_list:
    | expression; other_expr*                                              { $1 :: $2 }
    ;

other_expr:
    | COMMA; expression                                                    { $2 }
    ;

const_expr:
    | expression                                                           { $1 }
    ;

unary_op:
    | AMBER                                                                { UnaryRef }
    | MULTI                                                                { UnaryDeref }
    | PLUS                                                                 { UnaryPlus }
    | MINUS                                                                { UnaryMinus }
    | NOT                                                                  { UnaryNot }
    ;

binary_op:
    | DIV                                                                  { BinDiv }
    | MULTI                                                                { BinMulti }
    | MOD                                                                  { BinMod }
    | PLUS                                                                 { BinPlus }
    | MINUS                                                                { BinMinus }
    | LESS                                                                 { BinLess }
    | GREATER                                                              { BinGreater }
    | LESSEQ                                                               { BinLessEq }
    | GREATEREQ                                                            { BinGreaterEq }
    | EQ                                                                   { BinEq }
    | NOTEQ                                                                { BinNotEq }
    | AND                                                                  { BinAnd }
    | OR                                                                   { BinOr }
    | COMMA                                                                { BinComma }
    ;

unary_assign:
    | PLUSPLUS                                                             { UnaryPlusPlus }
    | MINUSMINUS                                                           { UnaryMinusMinus }
    ;

binary_assign:
    | ASSIGN                                                               { BinAssign }
    | ASSIGN_MULTI                                                         { BinAssignMulti }
    | ASSIGN_DIV                                                           { BinAssignDiv }
    | ASSIGN_MOD                                                           { BinAssignMod }
    | ASSIGN_PLUS                                                          { BinAssignPlus }
    | ASSIGN_MINUS                                                         { BinAssignMinus }
    ;
