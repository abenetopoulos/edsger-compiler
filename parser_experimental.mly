%token <int> INT
%token <float> DOUBLE
%token <string> ID
%token <char> CHAR
%token <string> STRING
%token EOF
%token INT_T
%token CHAR_T
%token BOOL_T
%token DOUBLE_T
%token SEMICOLON
%token COMMA
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
%token UNARY_AND
%token UNARY_MULTI
%token UNARY_PLUS
%token UNARY_MINUS
%token NOT
%token BINARY_MULTI
%token BINARY_DIVISION
%token BINARY_MODULO
%token BINARY_PLUS
%token BINARY_MINUS
%token BINARY_LESS
%token BINARY_GREATER
%token BINARY_LESSEQ
%token BINARY_GREATEREQ
%token BINARY_EQ
%token BINARY_NOTEQ
%token BINARY_AND
%token BINARY_OR
%token BINARY_COMMA
%token PLUSPLUS
%token MINUSMINUS
%token ASSIGN
%token ASSIGN_MULTI
%token ASSIGN_DIV
%token ASSIGN_MOD
%token ASSIGN_PLUS
%token ASSIGN_MINUS

%start <'a option> prog

(*
%right ASSIGN 
%left BINARY_OR
%left BINARY_AND
%nonassoc NOT
%nonassoc BINARY_GREATER BINARY_GREATEREQ BINARY_LESS BINARY_LESSEQ BINARY_EQ BINARY_NOTEQ
%left BINARY_PLUS BINARY_MINUS
%left BINARY_MULTI BINARY_DIVISION BINARY_MODULO
%nonassoc UNARY_MINUS UNARY_PLUS
*)
%nonassoc LOWER_ASSIGN
%nonassoc BETWEEN_ASSIGNS
%nonassoc QUESTION_MARK
%nonassoc HIGHER_ASSIGN
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%

prog:
    | declaration; declaration*; EOF                                       { None }
    | EOF { None }
    ;

declaration:
    | var_declaration                                                      { Printf.printf "Var decl\n"; None }
    | fun_declaration                                                      { None }
    | fun_definition                                                       { None }
    ;

var_declaration:
    | object_type; declarator; other_decs*; SEMICOLON                      { None }
    ;

other_decs:
    | COMMA; declarator;                                                   { None }

object_type:
    | basic_type; BINARY_MULTI*                                                   { None }
    ;

basic_type:
    | INT_T                                                                  { None }
    | CHAR_T                                                                 { None }
    | BOOL_T                                                                 { None }
    | DOUBLE_T                                                               { None }
    ;

declarator:
    | ID; array_def?                                                       { None }
    ;

array_def:
    | LEFT_BRACKET; const_expr; RIGHT_BRACKET                              { None }
    ;

fun_declaration:
    | VOID ; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; SEMICOLON { None }
    | object_type; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; SEMICOLON { None }
    ;

parameter_list:
    | parameter; other_params*                                             { None }
    ;

other_params:
    | COMMA; parameter                                                     { None }
    ;

parameter:
    | BYREF?; object_type; ID                                              { None }
    ;

fun_definition:
    | VOID; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; LEFT_CURL; declaration*; statement*; RIGHT_CURL                                                                 { None }
    | object_type; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; LEFT_CURL; declaration*; statement*; RIGHT_CURL                                                                 { None }
    ;

statement:
    | SEMICOLON                                                            { None }
    | expression; SEMICOLON                                                { None }
    | LEFT_CURL; statement*; RIGHT_CURL                                    { None }
    | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement                   { None } %prec LOWER_THAN_ELSE
    | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement; ELSE; statement  { None }
    | label; FOR; LEFT_PAREN; expression?; SEMICOLON; expression?; SEMICOLON; expression?; RIGHT_PAREN; statement                                                                { None }
    | CONTINUE; ID?; SEMICOLON                                             { None }
    | BREAK; ID?; SEMICOLON                                                { None }
    | RETURN; expression?; SEMICOLON                                       { None }
    ;

label:
    | ID; COLON                                                            { None }
    ;


expression:
    | ID                                                                   { Printf.printf "ID %s\n" $1; None}
    | LEFT_PAREN; expression; RIGHT_PAREN                                  { None }
    | TRUE                                                                 { None }
    | FALSE                                                                { None }
    | NULL                                                                 { None }
    | INT                                                                  { Printf.printf "INT %d\n" $1; None }
    | CHAR                                                                 { None }
    | DOUBLE                                                               { None }
    | STRING                                                               { None }
    | ID; LEFT_PAREN; expression_list?; RIGHT_PAREN                        { None }
    | expression; LEFT_BRACKET; expression; RIGHT_BRACKET                  { None }
    | unary_op; expression                                                 { None }
    | expression; binary_op; expression                                    { None }
    | expression; binary_assign; expression                                { None }
    | LEFT_PAREN; object_type; RIGHT_PAREN; expression                     { None }
    | expression; QUESTION_MARK; expression; COLON; expression             { None }
    | NEW; object_type; array_exp?                                         { None }
    | DELETE; expression                                                   { None } %prec BETWEEN_ASSIGNS
    | unary_assign; expression                                             { None } %prec HIGHER_ASSIGN
    | expression; unary_assign                                             { None } %prec LOWER_ASSIGN
    ;

array_exp:
    | LEFT_BRACKET; expression; RIGHT_BRACKET                              { None }
    ;

expression_list:
    | expression; other_expr*                                              { None }
    ;

other_expr:
    | COMMA; expression                                                    { None }
    ;

const_expr:
    | expression                                                           { None }
    ;

unary_op:
    | UNARY_AND                                                            { None }
    | UNARY_MULTI                                                          { None }
    | UNARY_PLUS                                                           { None }
    | UNARY_MINUS                                                          { None }
    | NOT                                                                  { None }
    ;

binary_op:
    | BINARY_MULTI                                                         { None }
    | BINARY_DIVISION                                                      { None }
    | BINARY_MODULO                                                        { None }
    | BINARY_PLUS                                                          { None }
    | BINARY_MINUS                                                         { None }
    | BINARY_LESS                                                          { None }
    | BINARY_GREATER                                                       { None }
    | BINARY_LESSEQ                                                        { None }
    | BINARY_GREATEREQ                                                     { None }
    | BINARY_EQ                                                            { None }
    | BINARY_NOTEQ                                                         { None }
    | BINARY_AND                                                           { None }
    | BINARY_OR                                                            { None }
    | BINARY_COMMA                                                         { None }
    ;

unary_assign:
    | PLUSPLUS                                                             { None }
    | MINUSMINUS                                                           { None }
    ;

binary_assign:
    | ASSIGN                                                               { None }
    | ASSIGN_MULTI                                                         { None }
    | ASSIGN_DIV                                                           { None }
    | ASSIGN_MOD                                                           { None }
    | ASSIGN_PLUS                                                          { None }
    | ASSIGN_MINUS                                                         { None }
    ;
