%{

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


open Base ;;
open Terms ;;


%}

%token LParL
%token LParR
%token LSemiCol

%token <string> LIdent
%token <string> LString
%token <int>    LInt
%token <bool>   LBool
%token LCst
%token LFun
%token LArr
%token LExt
%token LDbCol
%token LRec

%token LIf
%token LThen
%token LElse

%token LLet
%token LEq
%token LIn
%token LAnd

%token LEOF

%start ast
%type <Terms.ast> ast



%%
ast : externs app LEOF { Ast ((List.rev $1) , $2) }

externs :                          { [] }
 | externs LExt LIdent LInt LDbCol { Extern ($3,$4) :: $1 }

app:
 | simple                  { $1 }
 | app simple              { TApp ( $1 , [$2] ) }
 | app LSemiCol simple     { TSeq [$1 ; $3]     }
;


simple : 
   LBool                   { TDat (Bool $1)    }
 | LString                 { TDat (String $1 ) }
 | LInt                    { TDat (Int    $1 ) }
 | LIdent                  { TVar $1 }


 | LFun idents LArr simple             { TFun ((List.rev $2) , $4)  }
 | LParL app  LParR                    { $2              }
 | LLet  lets LIn simple               { TLetIn ( List.rev $2 , $4) }
 | LLet LRec LIdent LEq app LIn simple { TLetRecIn ($3,$5,$7)       }

 | LIf app LThen app LElse simple      { TIf ($2,$4,$6)             }
 ;

idents :
   LIdent                 { [ $1 ]   }
 | idents LIdent          { $2 :: $1 }
;

letunique :
   LIdent LEq app         { ( $1 , $3 ) }
;

lets :
   letunique              { [ $1 ]   }
 | lets LAnd letunique    { $3 :: $1 } 
;

%% 



