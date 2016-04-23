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

%start <'a option> prog

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
    | declaration; declaration*                                            { None }
    ;

declaration:
    | var_declaration                                                      { None }
    | fun_declaration                                                      { None }
    | fun_definition                                                       { None }
    ;

var_declaration:
    | object_type; declarator; other_decs*; SEMICOLON                      { None }
    ;

other_decs:
    | COMMA; declarator;                                                   { None }

object_type:
    | basic_type; MULTI*                                                   { None }
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
    | VOID ; ID; LEFT_PAREN; parameter_list?; RIGHT_PAREN; SEMICOLON       { None }
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
    | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement                   { None }     %prec LOWER_THAN_ELSE
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
    | ID                                                                   { None }
    | LEFT_PAREN; expression; RIGHT_PAREN                                  { None }
    | TRUE                                                                 { None }
    | FALSE                                                                { None }
    | NULL                                                                 { None }
    | INT                                                                  { None }
    | CHAR                                                                 { None }
    | DOUBLE                                                               { None }
    | STRING                                                               { None }
    | ID; LEFT_PAREN; expression_list?; RIGHT_PAREN                        { None }
    | expression; array_exp                                                { None } 
    | unary_op; expression                                                 { None }     %prec AMBER
    | expression; binary_op; expression                                    { None }     %prec BINARY
    | unary_assign; expression                                             { None }     %prec PLUSPLUS_PRE
    | expression; unary_assign                                             { None }
    | expression; binary_assign; expression                                { None }     %prec ASSIGN_MINUS
    | LEFT_PAREN; object_type; RIGHT_PAREN; expression                     { None }     %prec LEFT_PAREN
    | expression; QUESTION_MARK; expression; COLON; expression             { None }     %prec QUEST
    | NEW; object_type; array_exp?                                         { None }
    | DELETE; expression                                                   { None }
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
    | AMBER                                                                { None }
    | MULTI                                                                { None }
    | PLUS                                                                 { None }
    | MINUS                                                                { None }
    | NOT                                                                  { None }
    ;

binary_op:
    | MULTI                                                                { None } 
    | DIV                                                                  { None }
    | MOD                                                                  { None }
    | PLUS                                                                 { None }
    | MINUS                                                                { None }
    | LESS                                                                 { None }
    | GREATER                                                              { None }
    | LESSEQ                                                               { None }
    | GREATEREQ                                                            { None }
    | EQ                                                                   { None }
    | NOTEQ                                                                { None }
    | AND                                                                  { None }
    | OR                                                                   { None }
    | COMMA                                                                { None }
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
