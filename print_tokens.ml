open Parser
open Printf

(** UTF8 sequences for different symbols. *)
let lquote = "\xe2\x80\x98"
let rquote = "\xe2\x80\x98"
let rtriangle = "\xe2\x96\xb8"
let ltriangle = "\xe2\x97\x82"

(** [print_token show_loc out token] prints [token] on the out channel [out].
    If [show_loc] is true, prints position informations for the identifiers. *)
let print_token show_loc out = function
  | INT_CONST i ->
     fprintf out "INT_CONST %s%ld%s" lquote i rquote
  | BOOL_CONST b ->
     fprintf out "BOOL_CONST %s%s%s" lquote (string_of_bool b) rquote
  | INTEGER ->
     fprintf out "INTEGER"
  | BOOLEAN ->
     fprintf out "BOOLEAN"
  | IDENT id ->
     if show_loc then
       let pos = Location.startpos id in
       let l = pos.pos_lnum in
       let c = pos.pos_cnum - pos.pos_bol + 1 in
       fprintf out "IDENT %s%s%s %s line %d, char %d %s" lquote (Location.content id) rquote rtriangle l c ltriangle
     else
       fprintf out "IDENT %s%s%s" lquote (Location.content id) rquote
  | LET ->
     fprintf out "LET"
  | MUT ->
     fprintf out "MUT"
  | ADDR ->
     fprintf out "ADDR"
  | FUNC ->
     fprintf out "FUNC"
  | MAIN ->
     fprintf out "MAIN"
  | RETURN ->
     fprintf out "RETURN"
  | PLUS ->
     fprintf out "PLUS"
  | MINUS ->
     fprintf out "MINUS"
  | TIMES ->
     fprintf out "TIMES"
  | NOT ->
     fprintf out "NOT"
  | LT ->
     fprintf out "LT"
  | AND ->
     fprintf out "AND"
  | COMMA ->
     fprintf out "COMMA"
  | QUOT ->
     fprintf out "QUOT"
  | SEMICOLON ->
     fprintf out "SEMICOLON"
  | COLON ->
     fprintf out "COLON"
  | ASSIGN ->
     fprintf out "ASSIGN"
  | RET_TYPE ->
     fprintf out "RET_TYPE"
  | LPAREN ->
     fprintf out "LPAREN"
  | RPAREN ->
     fprintf out "RPAREN"
  | LBRACKET ->
     fprintf out "LBRACKET"
  | RBRACKET ->
     fprintf out "RBRACKET"
  | LBRACE ->
     fprintf out "LBRACE"
  | RBRACE ->
     fprintf out "RBRACE"
  | SELF ->
     fprintf out "SELF"
  | NEW ->
     fprintf out "NEW"
  | DOT ->
     fprintf out "DOT"
  | SYSO ->
     fprintf out "SYSO"
  | IF ->
     fprintf out "IF"
  | ELSE ->
     fprintf out "ELSE"
  | WHILE ->
     fprintf out "WHILE"
  | EOF ->
     fprintf out "EOF"

let print out lexbuf show_loc =
  let token = ref (Lexer.get_token lexbuf) in
  fprintf out "%a\n" (print_token show_loc) !token;
  while !token <> EOF do
    token := Lexer.get_token lexbuf;
    fprintf out "%a\n" (print_token show_loc) !token
  done