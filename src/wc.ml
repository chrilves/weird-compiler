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

open Process ;;

(************************
 *                      *
 *       MESSAGES       *
 *                      *
 ************************)

let version =
"WC 0.0.1\n
Licensed under the terms of the GPLv3+\n\
Copyright GTof\n\
\n"


let usagemsg    =
"\
Usage: " ^ (Filename.basename Sys.argv.(0)) ^ " OPTIONS

 -t, --TeX                : compiles to TeX
 -s, --shell              : compiles to POSIX shell

 -a, --ast                : print ast
 -c, --core               : compiles to core
 -l, --alpha              : compiles to alpha renamed core
 -m, --moved              : compiles to alpha core with moved variables
 -e, --eta                : compiles to eta reduced core
 -S, --super              : compiles to supercombinators
 -p, --pseudo             : compiles to pseudo code

 -i, --intput <file path> : input  file ( or - for stdio )
 -o, --output <file path> : output file ( or - for stdout)
 -v, --version            : prints version informations

"

let texdesc     = "compiles to TeX"
let shelldesc   = "compiles to POSIX shell"

let astdesc     = "print abstract syntax tree"
let coredesc    = "compiles to core"
let alphdesc    = "compiled to alpha renamed core"
let moveddesc   = "compiles to alpha renamed core with moved variables"
let etadesc     = "compiles to eta reduced core"
let superdesc   = "compiles to supercombinators"
let pseudodesc  = "compiled to pseudo code"

let versiondesc = "prints version information"
let inputdesc   = "input file path (or - for standard input)"
let outputdesc  = "output file path (or - for standard output)"



(************************
 *                      *
 *        PARAMS        *
 *                      *
 ************************)


let null _ = ()


(** Tells which method to run *)
type process = TeX
             | Shell
             | PAst
             | PCore
             | PAlpha
             | PMoved
             | PEta
             | PSuper
             | PPseudo
             | Version
             | Usage

(** reference that holds the method selected:
    by default: print usage
 *)
let process = ref Usage
let setProcess x _ = (process := x)

(** return the path to the executable wc
    to know where are tex and shell directories
 *)
let exedir () =
    let s = Sys.getcwd ()
 in let _ = Sys.chdir (Filename.dirname Sys.argv.(0))
 in let r = Sys.getcwd ()
 in (Sys.chdir s ; r)

(** Tells which method to run *)

(** reference that holds the input, output file
    and recursion support.
    by default: startard input and output
                perform recursion
 *)
let inputfile  = ref "-"
let outputfile = ref "-"

(************************
 *                      *
 *       PROCESSING     *
 *                      *
 ************************)

let null _ = ()

(** lift a function f that takes an ast
    to a function that parses the input file
 *)
let upIn f () = 
    let (c,close) = (match !inputfile with
                       | "-" -> (stdin , null)
                       | x   -> let c = open_in x
                                in (c , (fun () -> close_in c))
                    )
 in let ast = Parser.ast Lexer.lexer (Lexing.from_channel c)
 in let _   = close ()
 in f ast


(** lift a function that returns a string to
    a functions that outputs to the output file
 *)
let upOut f x =
    let (c,close) = (match !outputfile with
                       | "-" -> (stdout , null)
                       | x   -> let c = open_out x
                                in (c , (fun () -> close_out c))
                    )
 in (output_string c (f x) ; flush c ; close ())



(************************
 *                      *
 *         MAIN         *
 *                      *
 ************************)

(** check the manual of module Arg for details *)
let main () =
 let _ =  Arg.parse [ ( "-t"        , Arg.Unit (setProcess TeX    ) , texdesc    )
                    ; ( "--TeX"     , Arg.Unit (setProcess TeX    ) , texdesc    )

                    ; ( "-s"        , Arg.Unit (setProcess Shell  ) , shelldesc  )
                    ; ( "--shell"   , Arg.Unit (setProcess Shell  ) , shelldesc  )

                    ; ( "-a"        , Arg.Unit (setProcess PAst  )  , astdesc    )
                    ; ( "--ast"     , Arg.Unit (setProcess PAst  )  , astdesc    )

                    ; ( "-c"        , Arg.Unit (setProcess PCore  ) , coredesc   )
                    ; ( "--core"    , Arg.Unit (setProcess PCore  ) , coredesc   )

                    ; ( "-l"        , Arg.Unit (setProcess PAlpha ) , coredesc   )
                    ; ( "--alpha"   , Arg.Unit (setProcess PAlpha ) , coredesc   )

                    ; ( "-m"        , Arg.Unit (setProcess PMoved ) , coredesc   )
                    ; ( "--moved"   , Arg.Unit (setProcess PMoved ) , coredesc   )

                    ; ( "-S"        , Arg.Unit (setProcess PSuper ) , superdesc  )
                    ; ( "--super"   , Arg.Unit (setProcess PSuper ) , superdesc  )

                    ; ( "-e"        , Arg.Unit (setProcess PEta   ) , etadesc    )
                    ; ( "--eta"     , Arg.Unit (setProcess PEta   ) , etadesc    )

                    ; ( "-p"        , Arg.Unit (setProcess PPseudo) , pseudodesc )
                    ; ( "--pseudo"  , Arg.Unit (setProcess PPseudo) , pseudodesc )

                    ; ( "-i"        , Arg.Set_string inputfile      , inputdesc  )
                    ; ( "--input"   , Arg.Set_string inputfile      , inputdesc  )

                    ; ( "-o"        , Arg.Set_string outputfile     , outputdesc )
                    ; ( "--output"  , Arg.Set_string outputfile     , outputdesc )

                    ; ( "-v"        , Arg.Unit (setProcess Version) , versiondesc)
                    ; ( "--version" , Arg.Unit (setProcess Version) , versiondesc)
                    ]
                    null
                    usagemsg in
 let texget   s = Base.stringOfFile ((exedir ()) ^ "/tex/"   ^ s ^ ".tex") in
 let shellget s = Base.stringOfFile ((exedir ()) ^ "/shell/" ^ s         ) in
 let meth   = (match !process with
                  | TeX     -> upIn (processTeX   texget  )
                  | Shell   -> upIn (processShell shellget)
                  | PAst    -> upIn processAst
                  | PCore   -> upIn processCore
                  | PAlpha  -> upIn processAlpha
                  | PMoved  -> upIn processMoved
                  | PEta    -> upIn processEta
                  | PSuper  -> upIn processSuper
                  | PPseudo -> upIn processPseudo
                  | Version -> Base.cst version
                  | Usage   -> Base.cst usagemsg
             )
 in upOut meth ()
;;

main ()



