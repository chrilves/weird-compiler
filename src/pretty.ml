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

type pretty_state = { mutable pretty_out     : string list ;
                      mutable pretty_orig    : int         ;
                      mutable pretty_pos     : int         ;
                      mutable pretty_size    : int
                    }

let pretty_state out orig pos size = { pretty_out    = out
                                     ; pretty_orig   = orig
                                     ; pretty_pos    = pos
                                     ; pretty_size   = size
                                     }

type 'a pretty = { pretty : pretty_state -> 'a }

let pretty m = { pretty = m }
let unpretty { pretty = m } = m


let preturn x = pretty (fun _ -> x)
let pbind { pretty = m } f = { pretty = fun s ->
 let x = m s
 in unpretty (f x) s
}

let pthen   m n = pbind m (cst n)
let pmap    f m = pbind m (preturn <| f)
let pjoin     m = pbind m id
let (>>=)   m f = pbind m f
let (>>)    m n = pthen m n

let plift_io f x = { pretty = fun _ -> f x }


(*****************************************
 * FONCTIONS BAS NIVEAU (DANGEREUSES!!!) *
 *****************************************)

let pretty_fast_concat n l =
 let buff = String.make n ' ' in
 let rec aux p = function
   | []     -> ()
   | s :: q -> let m = String.length s
               in (String.blit s 0 buff p m ; aux (p + m) q) 
 in (aux 0 l ; buff)


let pretty_get = { pretty = fun s -> (s.pretty_out , s.pretty_orig , s.pretty_pos , s.pretty_size ) }
let pretty_put out orig pos size = { pretty = fun s ->
 ( s.pretty_out    <- out  ;
   s.pretty_orig   <- orig ;
   s.pretty_pos    <- pos  ;
   s.pretty_size   <- size
 )
}

let pretty_get_orig   = { pretty = fun s -> s.pretty_orig      }
let pretty_get_pos    = { pretty = fun s -> s.pretty_pos       }

let pretty_set_orig n = { pretty = fun s -> s.pretty_orig <- n }
let pretty_set_pos  n = { pretty = fun s -> s.pretty_pos  <- n }

let pretty_append str = { pretty = fun s ->
                           let n = String.length str
                           in if n > 0
                              then ( s.pretty_out  <- str :: s.pretty_out ;
                                     s.pretty_size <- s.pretty_size + n   ;
                                     s.pretty_pos  <- s.pretty_pos  + n
                                   )
                        }

let pretty_newline =
 pretty_get_orig                   >>= (fun n ->
 pretty_append "\n"                >>
 pretty_set_pos 0                  >>
 pretty_append (String.make n ' ')
)




(*****************************************
 * FONCTIONS HAUT NIVEU (A UTILISER)     *
 *****************************************)


let pretty_local m =
 pretty_get_orig   >>= (fun n ->
 m                 >>= (fun r ->
 pretty_set_orig n >>= (fun _ ->
 preturn r             )))


let pretty_push    = pretty_get_pos >>= pretty_set_orig
let pretty_origin m = pretty_local ( pretty_push >> m )

let pretty_string str =
 let n = String.length str
 and b = ref 0
 in
 let rec aux p = if p < n
                 then if str.[p] = '\n'
                      then    pretty_append (String.sub str !b (p - !b))
                           >> pretty_newline
                           >>= (fun _ -> (b := succ p ; aux (succ p)))
                      else aux (succ p)
                 else pretty_append (String.sub str !b (p - !b))
 in aux 0
 


let pretty_run { pretty = m } =
 let s = { pretty_out  = [] ;
           pretty_orig = 0  ;
           pretty_pos  = 0 ;
           pretty_size = 0
         }
 in (m s ;
     pretty_fast_concat s.pretty_size (List.rev s.pretty_out)
    )



let pretty_list neutre sep l =
 let rec aux = function
  | [ ]    -> neutre
  | [x]    -> x
  | x :: q -> x >> sep >>= (fun _ -> aux q)
 in aux l
  

(******************
 * PETIT TEST     *
 ******************)


let test = pretty_string "toto\nest\ndans " >>
           pretty_push                      >>
           pretty_string "le\ngarage "      >>
           pretty_local (pretty_push                  >>
                         pretty_string "où il\nfait " >>
                         pretty_push                  >>
                         pretty_string "du\nvelo "
                        )                  >>
           pretty_string "a\nroulettes\n"

