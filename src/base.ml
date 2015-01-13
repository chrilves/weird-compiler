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


(************************
 *                      *
 *     FONCTIONAL       *
 *                      *
 ************************)

let swap f x y = f y x
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y
let id  x = x
let cst x _ = x
let (|>) f g x = g (f x)
let (<|) f g x = f (g x)


let fst (x,_) = x
let snd (_,x) = x

(************************
 *                      *
 *       MAP/SET        *
 *                      *
 ************************)


module OrdString =
struct
 type t = string
 let compare = compare
end

module SetString = Set.Make ( OrdString )
module MapString = Map.Make ( OrdString )

let compare_int x y = if x < y
                      then -1
                      else if x = y
                           then 0
                           else 1

module OrdInt =
struct
 type t = int
 let compare x y = compare_int x y
end

module SetInt = Set.Make ( OrdInt )
module MapInt = Map.Make ( OrdInt )

(** Whereas OCaml 3.12, Ocaml 3.11 does not have Map.bindings *)
let mapIntBindings    m = MapInt.fold    (fun k v l -> (k,v) :: l) m []
let mapStringBindings m = MapString.fold (fun k v l -> (k,v) :: l) m []


(************************
 *                      *
 *       BASE2          *
 *                      *
 ************************)

(** TeX does not accept digits in command names
    strBaseTwo converts a natural to base 2 made
    of 'O' for O and 'I' for 1
 *)
let strBaseTwo x =
 let rec aux = function
  | n when n < 0 -> failwith "strBaseTwo accepts only non-negative integers"
  | 0 -> "O"
  | 1 -> "I"
  | n ->  (aux (n/2)) ^ (if n mod 2 = 0 then "O" else "I")
 in aux x


(************************
 *                      *
 *        LISTS         *
 *                      *
 ************************)


(** find the last element of a list *)
let findLast l =
 let rec aux acc = function
 | []     -> failwith "findLast: empty list"
 | [x]    -> (List.rev acc , x)
 | x :: q -> aux (x :: acc) q
 in aux [] l

let rec take n l = match (n , l) with
 | (n ,  _    ) when n < 0 -> failwith "take: n < 0"
 | (0 , _     )            -> []
 | (n , []    )            -> failwith "take: to short list"
 | (n , x :: q)            -> x :: (take (n-1) q)

let rec drop n l = match (n , l) with
 | (n , _     ) when n < 0 -> failwith "drop: n < 0"
 | (0 , l     )            -> l
 | (n , []    )            -> failwith "drop: to short list"
 | (n , _ :: q)            -> drop (n-1) q


(************************
 *                      *
 *       OPTION         *
 *                      *
 ************************)


let omap f = function
 | None   -> None
 | Some x -> Some (f x)


let isSome = function
 | None   -> false
 | Some _ -> true

(************************
 *                      *
 *       FILES          *
 *                      *
 ************************)

(** stringOfFile "toto"
    read the file toto and output it as a string
 *)
let stringOfFile s =
    let cin = try (open_in s) with _ -> failwith ("File " ^ s ^ " not found!")
 in let rec aux e = try (aux (e ^ (input_line cin) ^ "\n"))
                    with End_of_file -> (close_in cin ; e)
 in aux ""



(************************
 *                      *
 *       MONADS         *
 *                      *
 ************************)


(** apply a modadic function to a list *)
let genMapM ret bind f =
 let trickbind m f = Obj.magic bind m f in
 let rec aux = function
  | []     -> ret []
  | x :: q -> bind      (f x  ) (fun x' ->
              trickbind (aux q) (fun q' ->
              ret (x' :: q') ))
 in aux


(** iter a monadic function f on a list *)
let genIterM ret bind f =
 let rec aux = function
  | []       -> ret ()
  | (x :: q) -> bind (f x) (fun _ -> aux q)
 in aux
  

(** fold a monadic function f on a list *)
let genFoldlM ret bind f =
 let rec aux i = function
  |  []      -> ret i
  | (x :: q) -> bind (f i x) (fun j -> aux j q)
 in aux

