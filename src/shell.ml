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

let of_dat = function
 | Int    i     -> (string_of_int i)
 | String s     -> "\"" ^ s ^ "\""
 | Bool   true  -> "1"
 | Bool   false -> "0"

let of_arg = function
 | ADat d -> of_dat d
 | AVar i -> "\"$"    ^ (string_of_int i) ^ "\""
 | ATmp i -> "\"$TMP" ^ (string_of_int i) ^ "\""

let shell_of_name = function
 | NameStr s -> s
 | NameInt i -> "sc" ^ (string_of_int i)

let rec of_code x =
 let aux = function
  | CArg a           -> pretty_string ("ACC=" ^ (of_arg a))
  | CPush            -> pretty_string ("stackpush")
  | CPop i           -> pretty_string ("stackpop TMP" ^ (string_of_int i))
  | CMkClosure (i,n) -> pretty_string ("mkclosure " ^ (string_of_int i) ^ " " ^ (shell_of_name n))
  | CApply a         -> pretty_string ("apply " ^ (of_arg a))
  | CSave i          -> pretty_string ("TMP" ^ (string_of_int i) ^ "=\"$ACC\"")

  | CCall (n,l)      -> let l = List.map (fun x -> " " ^ (of_arg x)) l
                        in pretty_string ((shell_of_name n) ^ (List.fold_left (^) "" l))
  | CIf (t,e)        -> pretty_string   "if [ $ACC -ne 0 ]" >>
                        pretty_string "\nthen " >>
                        pretty_origin ( pretty_list (preturn ())
                                                    (pretty_string "\n")
                                                    (List.map of_code t)
                                      ) >>
                        pretty_string "\nelse " >>
                        pretty_origin ( pretty_list (preturn ())
                                                    (pretty_string "\n")
                                                    (List.map of_code e)
                                      ) >>
                        pretty_string "\nfi"
 in pretty_origin (aux x)


let of_entry e =
  let s = shell_of_name e.e_id
  in pretty_origin ( pretty_string  (  "##---------  "
                                    ^  s
                                    ^ " : "
                                    ^ (string_of_int e.e_arity)
                                    ^ "  ----------\n"
                                    ^ s
                                    ^ " () {\n"
                                   ) >>
                    pretty_string "    " >>
                    pretty_origin ( pmapM (fun x -> (of_code x) >> pretty_string "\n") e.e_body ) >>
                    pretty_string "\n}\n"
                   )

let of_prog (Prog (l,a)) =
    pretty_origin ( pmapM of_entry l                               >>
                    pretty_string "##---------  MAIN  ---------\n" >>
                    pretty_string (shell_of_name a)                >>
                    pretty_string "\n"
                  )


let of_prog_standalone f x =
   (f "lib")
 ^ (f "extern")
 ^ "
##########################
#                        #
#        PROGRAM         #
#                        #
##########################
"
 ^ (pretty_run (of_prog x))
