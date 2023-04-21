{
  open Lexing
  open Parser

  exception Error of string

  (** [newline lexbuf] updates the current line number [pos_lnum] of the [lexbuf]
      and the number of characters from the beginning of the file to the beginning
      of the current line [pos_bol].
   *)
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let digit = ['0'-'9']
let integer = digit+
let space = [' ' '\t' '\r']
let letter = ['a'-'z''A'-'Z''_']
let ident = letter (digit | letter)*

rule get_token = parse
  | "//" [^ '\n']* '\n'
  | '\n'      { newline lexbuf; get_token lexbuf }
  | space+    { get_token lexbuf }
  | "->"      { RET_TYPE }
  | "/*"      { comment lexbuf }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { TIMES }
  | "&&"      { AND }
  | "<"       { LT }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRACKET }
  | ']'       { RBRACKET }
  | '{'       { LBRACE }
  | '}'       { RBRACE }
  | '.'       { DOT }
  | '\"'      { QUOT }
  | ';'       { SEMICOLON }
  | ':'       { COLON }
  | '='       { ASSIGN }
  | ","       { COMMA }
  | "&"       { ADDR }
  | "true"    { BOOL_CONST true }
  | "false"   { BOOL_CONST false }
  | "i32"     { INTEGER }
  | "bool"    { BOOLEAN }
  | "fn"      { FUNC }
  | "let"     { LET }
  | "mut"     { MUT }
  | "main"    { MAIN }
  | "return"  { RETURN }
  | "new"     { NEW }
  | "self"    { SELF }
  | "println!"  { SYSO }
  | "!"       { NOT }
  | "if"    { IF }
  | "else"  { ELSE }
  | "while" { WHILE }
  | integer as i
      {
        try
          (** We want two's complement 32 bits integers.
              On a 64 bits architecture, Ocaml integers are 63 bits wide. *)
          INT_CONST (Int32.of_string i)
        with Failure _ ->
          raise (Error "Invalid integer constant")
      }
  | ident as id { IDENT (Location.make (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) id) }
  | "//" [^ '\n']* eof
  | eof     { EOF }
  | _ as c  { raise (Error ("Illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*/"    { get_token lexbuf }
  | '\n'    { newline lexbuf; comment lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Error ("Unterminated comment")) }
