{
    open Lexing
    open Printf    
    open Parser
    open Ast

    exception SyntaxError of string

    let next_line lexbuf = 
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
            }
}

let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f']
let const_char = ''' ('\\' ['n' 't' 'r' '0' '\\' '''] | ['a'-'z' 'A'-'Z' '0'-'9'] | '\\' 'x' hexdigit hexdigit) ''' 
let exp = ['e' 'E'] ['-' '+']? digit+
let int_in = digit+
let float_in = digit+ '.' digit+ exp?

let white = [' ' '\t']+
let newline = '\r' | '\n'
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let keyword = "bool" | "break" | "byref" | "char" | "continue" | "delete" | "double" | "else" | "for" | "false" | "if" | "int" | "new" | "NULL" | "return" | "true" | "void"

let op = ['=' '>' '<' '+' '-' '*' '/' '%' '&' '!' '?' ':' ','] | "==" | "!=" | ">=" | "<=" | "&&" | "||" | "++" | "--" | "+=" | "-=" | "*=" | "/=" | "%="
let separator = [';' '(' ')' '[' ']' '{' '}']

rule read incFiles= 
    parse
    | white {read incFiles lexbuf}
    | newline {next_line lexbuf; read incFiles lexbuf}
    | "//" [^ '\n']* {(*printf "Single line comment %S\n" (Lexing.lexeme lexbuf);*) read incFiles lexbuf}
    | "/*" {let _ = (read_comment lexbuf.lex_curr_p.pos_lnum incFiles (Buffer.create 80) lexbuf) in () (*printf "Comment \"/*%s*/\"\n" com*); read incFiles lexbuf}
    | keyword {
        match (Lexing.lexeme lexbuf) with
            "bool" -> BOOL_T
        |   "break" -> BREAK
        |   "byref" -> BYREF
        |   "char" -> CHAR_T
        |   "continue" -> CONTINUE
        |   "delete" -> DELETE
        |   "double" -> DOUBLE_T
        |   "else" -> ELSE
        |   "for" -> FOR
        |   "false" -> FALSE
        |   "if" -> IF
        |   "int" -> INT_T
        |   "new" -> NEW
        |   "NULL" -> NULL
        |   "return" -> RETURN
        |   "true" -> TRUE
        |   _ -> VOID
    }
    | id {
        ID (Lexing.lexeme lexbuf)
    }
    | const_char {
        let firstChar = (String.get (Lexing.lexeme lexbuf) 1) in
        if (firstChar = '\\') then
            let secondChar = (String.get (Lexing.lexeme lexbuf) 2) in
            if (secondChar = 'n') then
                CHAR '\n'
            else if (secondChar = 't') then
                CHAR '\t'
            else if (secondChar = 'r') then
                CHAR '\r'
            else if (secondChar = '0') then
                CHAR (Char.chr 0)
            else (*hex*) begin
                let aux c = 
                    let a = Char.code c in
                    if (a > 47 && a < 58) then
                        a - 48
                    else
                        a - 87
                in
                let firstDig = aux (String.get (Lexing.lexeme lexbuf) 3) in
                let secondDig = aux (String.get (Lexing.lexeme lexbuf) 4) in
                let charAscii = 16 * firstDig + secondDig in
                CHAR (Char.chr charAscii)
            end
        else
            CHAR firstChar
    }
    | int_in {
        INT (int_of_string (Lexing.lexeme lexbuf)) 
    }
    | float_in {
        DOUBLE (float_of_string (Lexing.lexeme lexbuf))
    }
    | '"' {
        let str = read_string incFiles (Buffer.create 80) lexbuf in
        STRING str
    }
    | "#include" {let lineOffset = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol - 8 in
                  let currentFileName = List.hd incFiles in
                  begin
                      if (lineOffset = 0) then begin
                          start_include incFiles lexbuf;
                          read incFiles lexbuf
                      end
                      else begin
                          printf "\x1b[31mError\x1b[0m - file '%s', line %d: #include directive should be at the beginning of a line. Exiting.\n" currentFileName lexbuf.lex_curr_p.pos_lnum;
                          exit 1
                      end
                  end
                  }
    | op {
        match (Lexing.lexeme lexbuf) with
            "=" -> ASSIGN
        |   ">" -> GREATER
        |   "<" -> LESS
        |   "+" -> PLUS
        |   "-" -> MINUS
        |   "*" -> MULTI
        |   "/" -> DIV
        |   "%" -> MOD
        |   "&" -> AMBER
        |   "!" -> NOT
        |   "?" -> QUESTION_MARK
        |   ":" -> COLON
        |   "," -> COMMA
        |   "==" -> EQ
        |   "!=" -> NOTEQ
        |   ">=" -> GREATEREQ
        |   "<=" -> LESSEQ
        |   "&&" -> AND
        |   "||" -> OR
        |   "++" -> PLUSPLUS
        |   "--" -> MINUSMINUS
        |   "+=" -> ASSIGN_PLUS
        |   "-=" -> ASSIGN_MINUS
        |   "*=" -> ASSIGN_MULTI
        |   "/=" -> ASSIGN_DIV
        |   "%=" -> ASSIGN_MOD
        |   _ -> ASSIGN_DIV
    }
    | separator { 
        match (Lexing.lexeme lexbuf) with
            ";" -> SEMICOLON
        |   "(" -> LEFT_PAREN
        |   ")" -> RIGHT_PAREN
        |   "[" -> LEFT_BRACKET
        |   "]" -> RIGHT_BRACKET
        |   "{" -> LEFT_CURL
        |   _ -> RIGHT_CURL
    }
    | eof {EOF}
    | _ {printf "\x1b[31mError\x1b[0m - file '%s', line %d: Invalid token '%s'.\n" (List.hd incFiles) lexbuf.lex_curr_p.pos_lnum (Lexing.lexeme lexbuf) ; exit 1}
and start_include incFiles=
    parse
    | [' ']+ '"' {let fileName = read_string incFiles (Buffer.create 80) lexbuf in 
           if (not (List.exists (fun x -> x = fileName) incFiles)) then
                let inF = open_in fileName in
                let lbuf = Lexing.from_channel inF in
                Parser.prog (read (fileName :: incFiles)) lbuf
           else
               let currentFileName = List.hd incFiles in 
               begin
               printf "\x1b[31mError\x1b[0m - file '%s', line %d: Cyclical \"#include\"s found.\n" currentFileName lexbuf.lex_curr_p.pos_lnum;
               exit 1
           end
          }
    | [' ']* '\n' {printf "\x1b[31mError\x1b[0m - file '%s', line %d: No file specified for \"#include\"\n" (List.hd incFiles) lexbuf.lex_curr_p.pos_lnum; exit 1}
    | _     {printf "\x1b[31mError\x1b[0m - file '%s', line %d: Invalid use of \"#include\"\n" (List.hd incFiles) lexbuf.lex_curr_p.pos_lnum; exit 1}
and read_string incFiles buf =
    parse
    | '"'       { (Buffer.contents buf)}
    | '\n'      { printf "\x1b[31mError\x1b[0m - file '%s', line %d: Missing terminating '\"' character.\n" (List.hd incFiles) lexbuf.lex_curr_p.pos_lnum; exit 1}
    | '\\' '/'  { Buffer.add_char buf '/'; read_string incFiles buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string incFiles buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string incFiles buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string incFiles buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string incFiles buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string incFiles buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string incFiles buf lexbuf }
    | [^ '"' '\\' '\n']+
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string incFiles buf lexbuf
        }
    | _ { printf "\x1b[31mError\x1b[0m - file '%s', line %d: Illegal string character: '%S'\n" (List.hd incFiles) lexbuf.lex_curr_p.pos_lnum (Lexing.lexeme lexbuf); exit 1}
    | eof {printf "\x1b[31mError\x1b[0m - file '%s', line %d: String is not terminated.\n"  (List.hd incFiles) lexbuf.lex_curr_p.pos_lnum; exit 1}
and read_comment startLine incFiles buf=
    parse
    | "*/"      {(Buffer.contents buf)}
    | _         {Buffer.add_string buf (Lexing.lexeme lexbuf); read_comment startLine incFiles buf lexbuf}
    | eof 
        { printf "\x1b[31mError\x1b[0m - file '%s', line %d: Comment starting on this line is not terminated.\n" (List.hd incFiles) startLine;
          exit 1}
