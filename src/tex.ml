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


open Base
open Terms
open Pretty

let rec tex_of_int = function
 | n when n < 0 -> failwith "tex_of_int: n < 0"
 | 0            -> "O"
 | 1            -> "I"
 | n            -> (tex_of_int (n/2)) ^ (if n mod 2 = 0 then "O" else "I")

let tex_of_name = function
 | NameStr s -> s
 | NameInt i -> "sc" ^ (tex_of_int i)

let of_dat = function
 | Int    i     -> (string_of_int i)
 | String s     -> "{" ^ s                 ^ "}"
 | Bool   true  -> "1"
 | Bool   false -> "0"

                       
let rec of_code = function
  | CArg (ADat a)    -> "\\gdef\\acc{" ^ (of_dat a) ^ "}%\n"

  | CArg (AVar i)    -> "\\gdef\\acc{#" ^ (string_of_int i) ^ "}%\n"
  | CArg (ATmp i)    -> "\\expandafter\\gdef\\expandafter\\acc\\expandafter{\\tmp" ^ (tex_of_int i) ^ "}%\n"

  | CPush            -> "\\stackpush%\n"
  | CPop i           -> "\\stackpop{\\tmp" ^ (tex_of_int i) ^ "}%\n"
  | CMkClosure (i,n) -> "\\mkclosure{" ^ (string_of_int i) ^ "}{\\" ^ (tex_of_name n) ^ "}%\n"

  | CApply (ADat a)  -> "\\apply{" ^ (of_dat a) ^ "}%\n"
  | CApply (AVar i)  -> "\\apply{#" ^ (string_of_int i) ^ "}%\n"
  | CApply (ATmp i)  -> "\\expandafter\\apply\\expandafter{\\tmp" ^ (tex_of_int i) ^ "}%\n"

  | CCall (n,l)      -> (List.fold_left (fun s x -> match x with
                                           | ADat a -> s ^ "{"  ^ (of_dat a       ) ^ "}"
                                           | AVar i -> s ^ "{#" ^ (string_of_int i) ^ "}"
                                           | ATmp i -> "\\expandafter\\swapa\\expandafter{\\tmp" ^ (tex_of_int i) ^ "}{" ^ s ^ "}"
                                        )
                                        ("\\" ^ (tex_of_name n))
                                        l
                        ) ^ "%\n"
  | CSave i          -> "\\global\\let\\tmp" ^ (tex_of_int i) ^ "=\\acc%\n"
  | CIf (t,e)        ->   "\\ifnum\\acc=0\n"
                        ^ (List.fold_left (^) "" (List.map of_code e))
                        ^ "\\else%\n"
                        ^ (List.fold_left (^) "" (List.map of_code t))
                        ^ "\\fi%\n"


let rec mkpat acc = function
   | 0 -> acc
   | x -> mkpat ("#" ^ (string_of_int x) ^ acc) (pred x)

let of_entry e =
  let s    = tex_of_name e.e_id
  and l    = List.map of_code e.e_body in
  let pat  = mkpat "" e.e_arity
  in   (  "%%---------  " ^ s ^ " : " ^ (string_of_int e.e_arity) ^ "  ----------\n"
        ^ "\\def\\" ^ s  ^ pat ^ "%\n{%\n"
        ^ (List.fold_left (^) "" l)
        ^ "}%\n%\n"
       )

let of_prog (Prog (l,a)) =
 let le = List.map of_entry l
 in (  (List.fold_left (^) "" le)  
     ^ "%%---------  MAIN  ---------\n"
     ^ "\\" ^ (tex_of_name a) ^ "%\n%\n"
    )

(** f is the fetching function *)
let of_prog_standalone f x =
   "\\tracingcommands=1%\n"
 ^ "\\tracingmacros=1%\n"
 ^ (f "lib")
 ^ (f "extern")
 ^ "%
% %%%%%%%%%%%%%%%%%%%%%
% %      PROGRAM      %
% %%%%%%%%%%%%%%%%%%%%%
%
%
"
 ^ (of_prog x)
 ^ "\\end\n"

