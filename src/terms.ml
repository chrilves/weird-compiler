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


(*****************************
 *                           *
 *   Abstract Syntax Tree    *
 *                           *
 *****************************)


(** primitive data types *)
type dat = Int    of int
         | Bool   of bool
         | String of string

(** type of variables *)
type var = string

(** type of parsed terms *)
type term = TVar      of var
          | TDat      of dat
          | TFun      of var list * term
          | TApp      of term * term list
          | TLetIn    of (var  * term) list * term
          | TLetRecIn of var * term * term
          | TSeq      of term list
          | TIf       of term * term * term

(** declarations of primitives *)
type extern = Extern of (string * int)

(** type of the Abstract-Syntax Tree *)
type ast = Ast of extern list * term


(*****************************
 *                           *
 *           Names           *
 *                           *
 *****************************)

(** Represents super combinators or variable id
    It can be a string or an integer (which will be transformed
    into a name later)
*)
type name = NameStr of string
          | NameInt of int

let nameStr x = NameStr x
let nameInt x = NameInt x

let stringOfName = function
  | NameStr s -> s
  | NameInt i -> string_of_int i

module OrdName =
struct
  type t = name
  let compare x y = match (x,y) with
   | (NameStr a  , NameStr b ) -> compare a b
   | (NameInt  a , NameInt b ) -> compare_int a b
   | (NameStr _  , NameInt  _) -> -1
   | (NameInt  _ , NameStr _ ) -> 1
end

module SetName = Set.Make(OrdName)
module MapName = Map.Make(OrdName)


(*************************
 *                       *
 *      Annotations      *
 *                       *
 *************************)


type annot = { mutable freevars : SetName.t option ;
               mutable isValue  : bool      option ;
               mutable isPure   : bool      option ;
             }

let mkAnnot f v p = { freevars = f ; isValue = v ; isPure = p }
let annotZero   = mkAnnot None None None


type 'a annotated = Annot of annot * 'a
let annotate x t = Annot (x,t)
let annotation (Annot (x,_)) = x
let annotTerm  (Annot (_,t)) = t

let annotMap f (Annot (a,t)) = Annot (a , f t)

(*************************
 *                       *
 *         CORE          *
 *                       *
 *************************)

type 'a atom = Var of 'a
             | Sup of name
             | Dat of dat

type 'a corerec = Atm of name atom
                | Fun of name * 'a
                | App of 'a * 'a
                | Rec of name * 'a
                | Seq of 'a list * 'a
                | If  of 'a * 'a * 'a

type core = Core of (core corerec) annotated
let core x = Core x

let coreMap f (Core x) = Core (annotMap f x)

let mkCore a t = Core ( Annot ( a , t ) )
let mkCoreZero t = mkCore annotZero t

let mkFun s c = Core ( Annot ( annotZero , Fun (s,c)   ) )
let mkApp c d = Core ( Annot ( annotZero , App (c,d)   ) )
let mkRec x c = Core ( Annot ( annotZero , Rec (x,c)   ) )
let mkSeq l c = Core ( Annot ( annotZero , Seq (l,c)   ) )
let mkAtm a   = Core ( Annot ( annotZero , Atm a       ) )

let mkIf c t e = Core ( Annot ( annotZero , If (c,t,e) ) )

let mkVar x   = mkAtm (Var x)
let mkDat x   = mkAtm (Dat x)
let mkSup x   = mkAtm (Sup x)

let mkVarStr n = mkVar (NameStr n)
let mkVarInt n = mkVar (NameInt n)

let mkSupStr n = mkSup (NameStr n)
let mkSupInt n = mkSup (NameInt  n)


let multiFun l = List.fold_right mkFun l
let multiApp c = List.fold_left  mkApp c


(** Access functions *)

let coreTerm  (Core (Annot (_,x))) = x
let coreAnnot (Core (Annot (x,_))) = x




(*************************
 *                       *
 *        SUPER          *
 *                       *
 *************************)

(** Type of super combinators *)
type 'a super = SApp of 'a super * 'a super list
              | SAtm of 'a atom
              | SSeq of 'a super list
              | SIf  of 'a super * 'a super * 'a super


let mkSuperVar x = SAtm (Var x)

(** normalise a super *)
let superNormalize t =
 let rec dispatch = function
    | SAtm a      -> SAtm a
    | SSeq l      -> superGroupSeq [] (List.map dispatch l)
    | SApp (c,l)  -> superGroupApp (List.map dispatch l) (dispatch c)
    | SIf (c,t,e) -> SIf ( dispatch c
                         , dispatch t
                         , dispatch e
                         )
 and superGroupSeq acc = function
    | []             -> (match List.rev acc with
                           | [] -> failwith "empty sequence of super"
                           | [x] -> x
                           | l   -> SSeq l
                        )
    | (SSeq l) :: l' -> superGroupSeq acc (l @ l')
    | y        :: l' -> superGroupSeq (y :: acc) l'
 and superGroupApp l = function
    | SApp (x , l2) -> superGroupApp (l2 @ l) x
    | y             -> (match l with
                         | [] -> y
                         | _  -> SApp ( y , l)
                       )
 in dispatch t



(** Is a supercombinator body a value *)
let rec superIsValue sup = match superNormalize sup with
 | SAtm a -> (match a with
                | Var _ -> None
                | _     -> Some a
             )
 | _      -> None


(** free vars of a super combinaor *)
let rec superFreeVars = function
  | SAtm a       -> (match a with
                      | Var x -> SetName.singleton x
                      | _     -> SetName.empty
                    )
  | SApp (s , l) -> List.fold_left SetName.union (superFreeVars s) (List.map superFreeVars l      )
  | SSeq l       -> List.fold_left SetName.union  SetName.empty    (List.map superFreeVars l      )
  | SIf  (c,t,e) -> List.fold_left SetName.union  SetName.empty    (List.map superFreeVars [c;t;e])


(** Apply a fonction on atoms of a super combinator *)
let rec superMap f = function
 | SAtm a -> SAtm (f a)
 | SApp (x , l) -> SApp (superMap f x , List.map (superMap f) l)
 | SSeq  l      -> SSeq (List.map (superMap f) l)
 | SIf (c,t,e)  -> SIf  (superMap f c , superMap f t , superMap f e)

(** replace variable names by variable number in the list of arguments sl
    If a variable is not found in sl then raise an error
  *)
let superAlpha sl =
    let (_,sigma) = List.fold_left (fun (n,m) s -> (succ n , MapName.add s n m))
                                   (1 , MapName.empty)
                                   sl
 in let subst = function
     | Dat z -> Dat z
     | Sup x -> Sup x
     | Var y -> Var (try (MapName.find y sigma)
                     with Not_found -> failwith ((stringOfName y) ^ " was not found")
                    )
 in superMap subst


(*************************
 *                       *
 *        Code           *
 *                       *
 *************************)

type arg  = ADat of dat
          | AVar of int
          | ATmp of int

type code = CArg       of arg
          | CPush
          | CPop       of int
          | CCall      of name * arg list
          | CMkClosure of int * name
          | CApply     of arg                   (* ACC = closure *)
          | CSave      of int
          | CIf        of code list * code list


(** converts an atom into an argument *)
let argOfAtom = function
 | Dat d -> ADat d
 | Var i -> AVar i
 | Sup _ -> failwith "argOfAtom: super combinator"



(*************************
 *                       *
 *        Entries        *
 *                       *
 *************************)


(** a super combinator or code definition
    se_id    : superId of the super combinator or code
    se_arity : arity   of the super combinator or code
    se_body  : body    of the super combinator or code
 *)
type 'a entry = { e_id    : name   ;
                  e_arity : int    ;
                  e_body  : 'a
                }
let entry a b c = { e_id = a ; e_arity = b ; e_body = c }


(** maps en entry *)
let entryMap f e = { e with e_body = f e.e_body }


type 'a prog = Prog of 'a entry list * name


(*************************
 *                       *
 *   PRETTY PRINTING     *
 *                       *
 *************************)


open Pretty

let pmapM   f   l = genMapM   preturn pbind f   l
let piterM  f   l = genIterM  preturn pbind f   l
let pfoldlM f i l = genFoldlM preturn pbind f i l

let string_of_dat = function
 | Int i      -> string_of_int i
 | Bool true  -> "true"
 | Bool false -> "false"
 | String s   -> "\"" ^ s ^ "\""

let rec pretty_term c = 
 let aux = function
 | TVar v     -> pretty_string v
 | TDat d     -> pretty_string (string_of_dat d)
 | TFun (l,t) -> pretty_string ("fun " ^ (List.fold_left (fun x y -> x ^ " " ^ y) "" l) ^ " -> ") >>
                 pretty_origin ( pretty_string "( " >>
                                 pretty_term t     >>
                                 pretty_string "\n)"
                               )
 | TApp (c,l) -> pretty_string "( " >>
                 pretty_term c     >>
                 pretty_string " "  >>
                 pretty_origin ( pretty_list (preturn ())
                                             (pretty_string "\n")
                                             (List.map pretty_term l)
                               ) >>
                 pretty_string " )"

 | TLetIn (l , t) -> pretty_string "let " >>
                     pretty_list (preturn ())
                                 (pretty_string "and ")
                                 (List.map (fun (v,t) -> pretty_string v     >>
                                                         pretty_string " = " >>
                                                         pretty_term  t     >>
                                                         pretty_string "\n"
                                           )
                                            l
                                 ) >>
                     pretty_string "in " >>
                     pretty_origin ( pretty_string "( " >>
                                     pretty_term t     >>
                                     pretty_string "\n)"
                                   )

  | TLetRecIn (v,s,t) -> pretty_string ("let rec " ^ v ^ " = " ) >>
                         pretty_term s                           >>
                         pretty_string "\nin "                   >>
                         pretty_origin ( pretty_string "("       >>
                                         pretty_term t           >>
                                         pretty_string "\n)"
                                       )

  | TSeq l -> pretty_list (preturn ())
                          (pretty_string " ;\n")
                          (List.map pretty_term l)

  | TIf (c,t,e) -> pretty_string "if "     >>
                   pretty_term c           >>
                   pretty_string "\nthen " >>
                   pretty_term t           >>
                   pretty_string "\nelse " >>
                   pretty_term e

 in pretty_origin (aux c)

let pretty_extern (Extern (s,i)) = pretty_string ("extern " ^ s ^ " " ^ (string_of_int i) ^ " ;;\n")  
let pretty_ast (Ast (l,t)) = pretty_origin ( piterM pretty_extern l >> pretty_term t)


let pretty_atom f = function
 | Var s -> pretty_string ("v:" ^ (f s))
 | Sup s -> pretty_string ("s:" ^ (stringOfName s))
 | Dat z -> pretty_string ("d:" ^ (string_of_dat z))



let rec pretty_core c =
 let aux c = match coreTerm c with
 | Atm a      -> pretty_atom stringOfName a
 | Fun (s,t)  -> pretty_string ("( fun " ^ (stringOfName s) ^ " -> ") >>
                 pretty_core t >>
                 pretty_string "\n)"

 | App (s,t)  -> pretty_string "( "     >>
                 pretty_core s          >>
                 pretty_string "\n   "  >>
                 pretty_core t          >>
                 pretty_string "\n)"

 | Rec (s,t)  -> pretty_string ("( rec " ^ (stringOfName s) ^ " ") >>
                 pretty_core t                                      >>
                 pretty_string "\n)"

 | Seq ([],t) -> pretty_core c
 | Seq (l ,t) -> pretty_string "( " >>
                 pretty_origin ( piterM (fun s -> pretty_core s >> pretty_string " ;\n") l >>
                                 pretty_core t
                               ) >>
                 pretty_string "\n)"
 | If (c,t,e) -> pretty_string "if "        >>
                 pretty_core c              >>
                 pretty_string "\n  then "  >>
                 pretty_core t              >>
                 pretty_string "\n  else "  >>
                 pretty_core e

 in pretty_origin (aux c)


let rec pretty_super f u =
 let aux = function
 | SAtm a        -> (* pretty_string "( an atom : " >> *)
                    pretty_atom f a
                    (* >> pretty_string " ) " *)
 | SApp (c , cl) -> (* pretty_string "( an app : " >> *)
                    pretty_super f c    >>
                    pretty_string " "  >>
                    pretty_origin ( pretty_list (preturn ())
                                                (pretty_string "\n")
                                                (List.map (pretty_super f) cl)
                                  )
                    (* >> pretty_string " ) " *)
                    

 | SSeq l        -> (* pretty_string " ( a seq : " >> *)
                    pretty_list (preturn ())
                                (pretty_string " ; \n")
                                (List.map (pretty_super f) l)
                    (* >> pretty_string " ) " *)
 | SIf (c,t,e)  -> pretty_string "sif "       >>
                   pretty_super f c           >>
                   pretty_string "\n  then "  >>
                   pretty_super f t           >>
                   pretty_string "\n  else "  >>
                   pretty_super f e
 in pretty_origin (aux u)



let string_of_arg = function
 | ADat d -> string_of_dat d
 | AVar i -> "$var" ^ (string_of_int i)
 | ATmp i -> "$tmp" ^ (string_of_int i)

let rec string_of_arg_list = function
 | []     -> ""
 | [x]    -> string_of_arg x
 | x :: q -> (string_of_arg x) ^ " , " ^ (string_of_arg_list q)

let rec pretty_code x =
 let rec aux = function
  | CArg a           -> pretty_string ("$acc <- " ^ (string_of_arg a))
  | CPush            -> pretty_string "push"
  | CPop i           -> pretty_string ("pop " ^ (string_of_int i))
  | CCall (n,l)      -> pretty_string ((stringOfName n) ^ " ( " ^ (string_of_arg_list l) ^ " )")
  | CMkClosure (i,n) -> pretty_string ("mkclosure ( " ^ (string_of_int i) ^ " , " ^ (stringOfName n) ^ " )")
  | CApply arg       -> pretty_string ("apply ( " ^ (string_of_arg arg) ^ " )")
  | CSave i          -> pretty_string ("save " ^ (string_of_int i))
  | CIf (t,e)        -> pretty_string    "if $acc"  >>
                        pretty_string  "\n  then "  >>
                        pretty_origin (pretty_list (preturn ())
                                                   (pretty_string "\n")
                                                   (List.map pretty_code t)
                                      ) >>
                        pretty_string  "\n  else " >>
                        pretty_origin (pretty_list (preturn ())
                                                   (pretty_string "\n")
                                                   (List.map pretty_code e)
                                      )

 in pretty_origin (aux x)



let pretty_codes l = pretty_origin ( pmapM (fun c -> pretty_code c >>
                                                     pretty_string "\n"
                                           )
                                           l
                                   )


let pretty_entry f { e_id = n ; e_arity = i ; e_body = b } =
 pretty_string ((stringOfName n) ^ " : " ^ (string_of_int i) ^ " = ") >>
 pretty_origin (f b)
               

let pretty_entries f l =
  pretty_origin ( pmapM (fun s -> pretty_entry f s >> pretty_string "\n") l)


let pretty_prog f (Prog (l,main)) =
  pretty_origin ( pretty_entries f l >>
                  pretty_string ("=> Main is " ^ (stringOfName main))
                )

