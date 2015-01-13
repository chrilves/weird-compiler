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


(************************
 *                      *
 *     STATE MONAD      *
 *                      *
 ************************)

type ('s,'a) stateM = StateM of ('s -> ('a *'s))
let stateM x            = StateM x
let unStateM (StateM x) = x
let srun m x = fst (unStateM m x)
let slocal (StateM m) = StateM (fun s -> (fst (m s) , s))

let sreturn x           = StateM (fun s -> (x,s))
let sbind (StateM m) f  = StateM (fun s -> let (x,s2) = m s in unStateM (f x) s2)
let sthen x y           = sbind x (fun _ -> y)

let sget                = StateM (fun s -> (s,s))
let sput s              = StateM (fun _ -> ((),s))

let sgets   f = sbind sget (fun x -> sreturn (f x))
let smodify f = sbind sget (fun x -> sput    (f x))


let (>>=) = sbind
let (>>)  = sthen


let smapM   f   l = genMapM   sreturn sbind f   l
let siterM  f   l = genIterM  sreturn sbind f   l
let sfoldlM f i l = genFoldlM sreturn sbind f i l




