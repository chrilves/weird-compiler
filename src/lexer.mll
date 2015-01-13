(*
    Copyright (C) 2012 Christophe Calvès

    This file is part of WC.

    WC is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    WC is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*)


{

open Parser ;;

}

let open_comment = "(*"
let close_comment = "*)"

rule lexer = parse
   [' ' '\t' '\n' '\r' ]                    { lexer lexbuf                  }
 | open_comment                             { comment 1 lexbuf }
 | "true"                                   { LBool true                    }
 | "false"                                  { LBool false                   }
 | "fun"                                    { LFun                          }
 | "->"                                     { LArr                          }
 | "-&gt;"                                  { LArr                          }
 | '='                                      { LEq                           }
 | "let"                                    { LLet                          }
 | "rec"                                    { LRec                          }
 | "in"                                     { LIn                           }
 | "and"                                    { LAnd                          }
 | "extern"                                 { LExt                          }
 | "if"                                     { LIf                           }
 | "then"                                   { LThen                         }
 | "else"                                   { LElse                         }
 | ";;"                                     { LDbCol                        }
 | '('                                      { LParL                         }
 | ')'                                      { LParR                         }
 | ';'                                      { LSemiCol                      }
 | '"'[^'"']*'"'                            { let s = Lexing.lexeme lexbuf
                                              in LString (String.sub s 1 ((String.length s) - 2))
                                            }
 | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']* { LIdent (Lexing.lexeme lexbuf) }
 | '-'?['0'-'9']+                           { LInt (int_of_string (Lexing.lexeme lexbuf))}
 | eof                                      { LEOF                          }

and comment depth = parse
 | open_comment  { comment (depth+1) lexbuf }
 | close_comment { if depth = 1
                   then lexer lexbuf
                   else comment (depth - 1) lexbuf
                 }
 | _             { comment depth lexbuf }

{
}
