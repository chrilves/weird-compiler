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

open Terms
open Comp



(************************
 *                      *
 *    COMPILATION       *
 *                      *
 ************************)

(** Print the ast
 *)
let processAst x = Pretty.pretty_run (pretty_ast x)


(** Compliled to core
 *)
let processCore x = Pretty.pretty_run (pretty_core (crun (coreOfAst x)))


(** Compliled to alpha renamed core
 *)
let processAlpha x =
  let m = coreOfAst x >>= coreAlpha
  in Pretty.pretty_run (pretty_core (crun m))



(** Compliled to reduced core
 *)
let processMoved x =
 let m = coreOfAst x                >>= (fun y ->
         coreAlpha (coreMoveVars y) >>= (fun u ->
         let _ = checkClos u in
         creturn u ) )
 in Pretty.pretty_run (pretty_core (crun m))


(** Compliled to eta reduced core
 *)
let processEta x =
  let m = coreOfAst x                  >>= (fun y ->
          coreAlpha (coreMoveVars y)   >>= (fun u ->
          let _ = checkClos u in
          cgets ceArity                >>= (fun arr ->
          creturn (coreEtaReduce arr u) )))
  in Pretty.pretty_run (pretty_core (crun m))



(** compiles an ast to a core *)
let core_of_ast x =
  coreOfAst x                >>= (fun y ->
  coreAlpha (coreMoveVars y) >>= (fun u   ->
  let _ = checkClos u in
  creturn u       >>= (fun t   ->
  cgets ceArity   >>= (fun arr ->
  compileCore (coreEtaReduce arr t) ))))

(** Compliled to super combinators
 *)
let processSuper x =
  let m = core_of_ast x >>= (fun main ->
          superCombinators >>= (fun prog ->
          creturn (Prog (prog , main)) ))
  in Pretty.pretty_run (pretty_prog (pretty_super string_of_int) (crun m))



(** Compliled to pseudo code
 *)
let processPseudo x =
  let m = core_of_ast x >>= (fun main ->
          codesOpt >>= (fun prog ->
          creturn (Prog (prog , main)) ))
  in Pretty.pretty_run (pretty_prog pretty_codes (crun m))



(** compiles the input file to TeX
 *)
let processTeX f x =
  let m = core_of_ast x >>= (fun main ->
          codesOpt >>= (fun prog ->
          creturn (Prog (prog , main)) ))
 in Tex.of_prog_standalone f (crun m)



(** compiles the input file to shell
 *)
let processShell f x =
 let m = core_of_ast x >>= (fun main ->
         codesOpt >>= (fun prog ->
         creturn (Prog (prog , main)) ))
 in Shell.of_prog_standalone f (crun m)



