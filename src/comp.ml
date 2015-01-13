open Base ;;
open Terms ;;

(*****************************
 *                           *
 *     Compilation Monad     *
 *                           *
 *****************************)


(** compilation environment
    ce_nextid: used to obtain fresh super-combinator names
    ce_supers: map of super combinators
    ce_arity : maps every super combinator or primtive to its arity
    ce_names : mapping from super combinator number to name
    ce_code  : already generated code (reverse order!)
 *)
type compEnv = { mutable ce_nextvarid : int ;
                 mutable ce_nextsupid : int ;
                 mutable ce_supers    : int super  MapName.t ;
                 mutable ce_arity     : int        MapName.t ;
                 mutable ce_names     : var        MapInt.t  ;
                 mutable ce_namesinv  : int        MapString.t ;
                 mutable ce_code      : code list
               }
(** creation function *)
let compEnv j i s a n l c = { ce_nextvarid = j ;
                             ce_nextsupid = i ;
                             ce_supers    = s ;
                             ce_arity     = a ;
                             ce_names     = n ;
                             ce_namesinv  = l ;
                             ce_code      = c
                           }


let ceNextVarId x = x.ce_nextvarid
let ceNextSupId x = x.ce_nextsupid
let ceSupers    x = x.ce_supers
let ceArity     x = x.ce_arity
let ceNames     x = x.ce_names
let ceNamesInv  x = x.ce_namesinv
let ceCode      x = x.ce_code


(** empty compilation environment *)
let compEnvZero () = compEnv 0 0 MapName.empty MapName.empty MapInt.empty MapString.empty []


(** almost like a state monad, but compEnv is made of mutable fields
    so we can alter them instead of passing a new state
 *)
type 'a compM = CompM of (compEnv -> 'a)
let compM x = CompM x
let unCompM (CompM x) = x

let creturn x = CompM (fun _ -> x)
let cbind (CompM m) f = CompM (fun c ->    let n = m c
                                        in let CompM m' = f n
                                        in m' c
                              )
let cthen x y = cbind x (fun _ -> y)

(** run the compilation monad *)
let crun (CompM m) = m (compEnvZero ())


let (>>=) = cbind
let (>>)  = cthen


let cgets = compM
 let cgets f = CompM (fun x -> f x) 

(** monad utils *)
let cmapM   f   l = genMapM   creturn cbind f   l
let citerM  f   l = genIterM  creturn cbind f   l
let cfoldlM f i l = genFoldlM creturn cbind f i l

(** get new super id *)
let getNewSuperId = CompM (fun c -> (c.ce_nextsupid <- c.ce_nextsupid + 1 ; NameInt c.ce_nextsupid))

(** get new var id *)
let getNewVarId   = CompM (fun c -> (c.ce_nextvarid <- c.ce_nextvarid + 1 ; NameInt c.ce_nextvarid))


(** try to set a constant name *)
let trySetName i z = CompM (fun c ->
  if    (MapInt.mem    i          c.ce_names)  (* super combinator already registerd *)
     || (MapName.mem  (NameStr z) c.ce_arity)  (* name of a prmititive               *)
     || (String.length z < 3                )  (* too short name                     *)
  then NameInt i
  else ( c.ce_names    <- MapInt.add    i z c.ce_names   ;
         c.ce_namesinv <- MapString.add z i c.ce_namesinv ;
         NameStr z
       )
)


(** get the arity of a super combinator ... maybe *)
let getArity superid = cgets id >>= (fun c ->
                       let name = match superid with
                                   | NameStr s -> if MapString.mem s c.ce_namesinv
                                                  then NameInt (MapString.find s c.ce_namesinv)
                                                  else superid
                                   | _         -> superid
                       in if MapName.mem name c.ce_arity
                          then creturn (Some (MapName.find name c.ce_arity))
                          else creturn  None
                       )

(** register the arity of a super combinator or primitive *)
let registerArity superid n =
   cgets id >>= (fun ce ->
   if MapName.mem superid ce.ce_arity
   then let x = MapName.find superid ce.ce_arity
        in if x = n
           then creturn ()
           else failwith (   (stringOfName superid)
                           ^ " already defined with arity "
                           ^ (string_of_int x)
                           ^ " and not "
                           ^ (string_of_int n)
                           ^ "!"
                         )
   else ( ce.ce_arity <- MapName.add superid n ce.ce_arity ;
          creturn ()
        )
   )

(** register arity of an extern *)
let registerExtern (Extern (s,i)) = registerArity (NameStr s) i


(*************************
 *                       *
 *     Primities         *
 *                       *
 *************************)

let ifcst  = "wcif"
let ifarr  = 3


let primitives = [ Extern (ifcst  , ifarr )
                 ]

(* recusrion *)

let reccst = "rec"


(*****************************
 *                           *
 *       AST to Core         *
 *                           *
 *****************************)

(** group sequeces *)
let rec groupSeq l = function
 | []             -> failwith "empty sequence"
 | [TSeq l2]      -> groupSeq l l2
 | [x      ]      -> (List.rev l , x)
 | (TSeq l2) :: q -> groupSeq l (l2 @ q)
 |  x        :: q -> groupSeq (x :: l) q

(** core of a term without Sup !!! *)
let rec coreOfTerm = function
 | TVar x            -> creturn (mkVarStr x)
 | TDat y            -> creturn (mkDat y)
 | TFun   (l,c)      -> coreOfTerm c >>= (fun c' -> creturn (multiFun (List.map nameStr l) c'))
 | TApp   (c,l)      -> coreOfTerm c       >>= (fun c' ->
                        cmapM coreOfTerm l >>= (fun l' ->
                        creturn ( multiApp c' l' )
                        ))
 | TLetIn (l,c)      -> let (n,e) = List.split l
                        in coreOfTerm c       >>= (fun c' ->
                           cmapM coreOfTerm e >>= (fun e' ->
                           creturn (multiApp (multiFun (List.map nameStr n) c') e')
                           ))
 | TLetRecIn (x,s,t) -> coreOfTerm s >>= (fun s' ->
                        coreOfTerm t >>= (fun t' ->
                        let sid = NameStr x in
                        creturn ( mkApp (mkFun sid t') (mkRec sid s')) ))
 | TSeq    l         ->    let (l2 , c ) = groupSeq [] l
                        in coreOfTerm c        >>= (fun c' ->
                           cmapM coreOfTerm l2 >>= (fun l' ->
                           creturn (mkSeq l' c')
                           ))
 | TIf  (c,t,e)      -> coreOfTerm c >>= (fun c' ->
                        coreOfTerm t >>= (fun t' ->
                        coreOfTerm e >>= (fun e' ->
                        creturn (mkIf c' t' e') )))
                        
(*
 | TIf (c,t,e)       -> coreOfTerm (TApp ( TVar ifcst
                                         , [ c
                                           ; TFun ([""] , t)
                                           ; TFun ([""] , e)
                                           ]
                                         )
                                   )
*)

(** core of Ast *)
let coreOfAst (Ast (el,t)) = cmapM registerExtern (primitives @ el) >> coreOfTerm t


(*****************************
 *                           *
 *     Core Manipulation     *
 *                           *
 *****************************)


let rec freeVar c = match coreTerm c with
  | Atm ( Var s ) -> SetName.singleton s
  | Atm ( Sup _ ) -> SetName.empty
  | Atm ( Dat _ ) -> SetName.empty
  | App (c1 , c2) -> SetName.union (freeVar c1) (freeVar c2)
  | Fun (s  , c ) -> SetName.remove s (freeVar c)
  | Rec (s  , c ) -> SetName.remove s (freeVar c)
  | Seq (l  , c ) -> let f s x = SetName.union s (freeVar x)
                     in SetName.union (List.fold_left f SetName.empty l) (freeVar c)
  | If  (c, t, e) -> List.fold_left SetName.union SetName.empty (List.map freeVar [c;t;e])



let rec freeVarMem c =
 let ano = coreAnnot c
 in (match ano.freevars with
       | None   -> let z = ( match coreTerm c with
                               | Atm ( Var s ) -> SetName.singleton s
                               | Atm ( Sup _ ) -> SetName.empty
                               | Atm ( Dat _ ) -> SetName.empty
                               | App (c1 , c2) -> SetName.union (freeVarMem c1) (freeVarMem c2)
                               | Fun (s  , c ) -> SetName.remove s (freeVarMem c)
                               | Rec (s  , c ) -> SetName.remove s (freeVarMem c)
                               | Seq (l  , c ) -> let f s x = SetName.union s (freeVarMem x)
                                                  in SetName.union (List.fold_left f SetName.empty l) (freeVarMem c)
                               | If  (c, t, e) -> List.fold_left SetName.union SetName.empty (List.map freeVar [c;t;e])

                           )
                   in ( ano.freevars <- Some z ; z )
       | Some x -> x
    )

(** is a core a direct value? *)
let rec isCoreValue arr c =
 let rec aux n c = match coreTerm c with
  | Atm ( Var s ) -> if n = 0
                     then true
                     else false
  | Atm ( Sup s ) -> if MapName.mem s arr
                     then MapName.find s arr > n
                     else failwith ("isCoreValue: " ^ (stringOfName s) ^ " not found")
  | Atm ( Dat _ ) -> true
  | Fun (s  , c ) -> true
  | Rec (s  , c ) -> aux n c
  | Seq ([] , c ) -> aux n c
  | Seq (_  , _ ) -> false
  | App (c1 , c2) -> if aux 0 c2
                     then aux (n+1) c1
                     else false
  | If (_,_,_)    -> false
 in aux 0 c




(** eta reduction on core
    true  = continue
    false = stop
 *)
let eta arr t = match coreTerm t with
 | Fun ( s , x ) -> ( match coreTerm x with
                        | App ( c , y ) -> (match coreTerm y with
                                               | Atm ( Var s' ) -> if (s = s')
                                                                      &&
                                                                      (not (SetName.mem s (freeVar c)))
                                                                      &&
                                                                      (isCoreValue arr c)
                                                                   then ( true ,  c )
                                                                   else ( false , t )
                                               | _              -> ( false , t )
                                           )
                        | _             -> (false , t ) 
                    )
 | _   -> ( false , t )



(** move variables:

      (fun x -> e0 e1) e2 => e0 ((fun x -> e1) e2)
      if x not in freeVar e0


    true  = continue
    false = stop
 *)
let moveVarsOneStep c = match coreTerm c with
 | App ( s , t ) -> ( match coreTerm s with
                        | Fun ( x , r ) -> (match coreTerm r with
                                               | App ( u , w )
                                                    when not (SetName.mem x (freeVar u))
                                                      -> (true , mkApp u (mkApp (mkFun x w) t))
                                               | _    -> (false, c)
                                           )
                        | _             -> (false , c ) 
                    )
 | _   -> ( false , c )


(** appy recursively a fonction such as eta on a core
    if the function returns true, the process continues
    otherwise it stops (see eta)
 *)
let rec process f t =
 let t'   = match coreTerm t with
              | Atm a         -> mkAtm a
              | App ( c , d ) -> mkApp   (process f c) (process f d)
              | Fun ( s , c ) -> mkFun s (process f c)
              | Rec ( s , c ) -> mkRec s (process f c)
              | Seq ( l , c ) -> mkSeq (List.map (process f) l) (process f c)
              | If  (c, t, e) -> mkIf (process f c) (process f t) (process f e)
 in let (b,u) = f t'
 in if b
    then process f u
    else u


(** eta reduction and move vars
 *)
let coreEtaReduce arr  x = process (eta arr)        x
let coreMoveVars       x = process  moveVarsOneStep x

(** alpha renaming + primitive detection
    all primitive and super combinator names must be in alreadyseen !!
  *)
let coreAlphaWith fv z =
 let alreadyseen = ref SetName.empty                           in
 let addSeen x   = (alreadyseen := SetName.add x !alreadyseen) in
 let isSeen  x   = (SetName.mem x !alreadyseen)                in

 let rec aux renaming c = match coreTerm c with
  (* It the Variable is to be renamed, then it is not a primitive, we rename it
     If it is not to be renamed, but is in the arity table, then it is not bound
     (otherwise Fun or Rec would have set it to be renamed). Then this is a Sup.
     Otherwise, this is just an free variable
   *)
  | Atm (Var v)   -> if MapName.mem v renaming
                     then let w = MapName.find v renaming
                          in creturn (mkAtm (Var w))
                     else getArity v >>= (fun oar ->
                          if isSome oar
                          then creturn (mkAtm (Sup v))
                          else creturn (mkAtm (Var v))
                          )
  | Atm a         -> creturn (mkAtm a)

  | App ( c , d ) -> aux renaming c >>= (fun c' ->
                     aux renaming d >>= (fun d' ->
                     creturn (mkApp c' d') ))

  | Seq ( l , c ) -> cmapM (aux renaming) l >>= (fun l' ->
                     aux renaming c         >>= (fun c' ->
                     creturn (mkSeq l' c') ))

  | Fun ( s , c ) -> if isSeen s
                     then getNewVarId                        >>= (fun vid ->
                          let _ = addSeen vid in
                          aux (MapName.add s vid renaming) c >>= (fun c'  ->
                          creturn (mkFun vid c') ))
                     else let _ = addSeen s in
                          aux renaming                     c >>= (fun c'  ->
                          creturn (mkFun s c') )
 
  | Rec ( s , c ) -> if isSeen s
                     then getNewVarId                        >>= (fun vid ->
                          let _ = addSeen vid in
                          aux (MapName.add s vid renaming) c >>= (fun c'  ->
                          creturn (mkRec vid c') ))
                     else let _ = addSeen s in
                          aux renaming                     c >>= (fun c'  ->
                          creturn (mkRec s c') )

  | If (c,t,e)    -> aux renaming c >>= (fun c' ->
                     aux renaming t >>= (fun t' ->
                     aux renaming e >>= (fun e' ->
                     creturn (mkIf c' t' e') )))


 in ( cgets ceArity >>= (fun aritytable ->
      ( alreadyseen := MapName.fold (fun key _ set -> SetName.add key set)
                                    aritytable
                                    fv ;
        aux MapName.empty z
      ) )
    )

let coreAlpha x = coreAlphaWith SetName.empty x


(** check that a term is closed or output the list of free vars as exception *)
let checkClos u =
 let fv = freeVar u
 in if SetName.is_empty fv
    then ()
    else let s = List.fold_left (fun s x -> s ^ " " ^ (stringOfName x))
                                "Term not closed! Free vars = "
                                (SetName.elements fv)
         in failwith s
         



(*****************************
 *                           *
 *     Substitution          *
 *                           *
 *****************************)


type subst = { subst_map : core MapName.t ;
               subst_fv  : int  MapName.t
             }

let subst m i = { subst_map = m ; subst_fv = i }
let substEmpty = subst MapName.empty MapName.empty

(** the set of the frevars of subst *)
let substFV s = MapName.fold (fun k v b -> if v > 0 then b else SetName.add k b)
                             s.subst_fv
                             SetName.empty

(** the union of two name maps with a combining function f
    If a key is present in the two maps, the result value for
    the key is f (value in img1) (value in img2)
 *)
let mapNameUnionWith f img1 img2 =
 MapName.fold (fun k v img -> MapName.add k (if MapName.mem k img
                                             then f v (MapName.find k img)
                                             else v
                                            )
                                            img
              )
              img1
              img2


(** Apply f on keys in set of map
 *)
let mapNameActOn f default set map =
 SetName.fold (fun k m -> MapName.add k (if MapName.mem k m
                                         then f (MapName.find k m)
                                         else default
                                        )
                                        m
              )
              set
              map



(** remove/add a mapping *)
let substRem x s =
  if MapName.mem x s.subst_map
  then let dec x = if x > 0 then x - 1 else 0
       and fv    = freeVar (MapName.find x s.subst_map)
       in subst (MapName.remove x s.subst_map) (mapNameActOn dec 0 fv s.subst_fv)
  else s

let substAdd x t s =
 let s' = substRem x s
 and fv = freeVar t
 in subst (MapName.add x t s'.subst_map) (mapNameActOn succ 1 fv s'.subst_fv)
 



(** substitution *)
let rec substitute s c = match coreTerm c with
 | Atm (Var v) -> if MapName.mem v s.subst_map
                  then MapName.find v s.subst_map
                  else mkAtm (Var v)
 | Atm a       -> mkAtm a
 | App (d,e)   -> mkApp (substitute s d) (substitute s e)
 | Fun (x,d)   -> mkFun x (substitute (substRem x s) d)
 | Rec (x,d)   -> mkRec x (substitute (substRem x s) d)
 | Seq (l,d)   -> mkSeq (List.map (substitute s) l) (substitute s d)
 | If  (d,e,f) -> mkIf (substitute s d) (substitute s e) (substitute s f)



(*****************************
 *                           *
 *     Super Combinators     *
 *                           *
 *****************************)


(** register a super combinator:
    registerSuper <list of arguments : var list> <body: super> <oname : string option>
    list of arguments: is the list the arguments of the super combinator
    body             : is the body of the super combinator
    oname            : if (Some name) then tries to register the super combinator with
                       this name, if it fails, it takes a fresh name.
    RETURNS: the superId of the registered super combinator
 *)
let registerSuper args body oname =
  cgets id >>= (fun ce ->
  let sid = ( match oname with
                | None      -> ( ce.ce_nextsupid <- succ ce.ce_nextsupid ; NameInt ce.ce_nextsupid )
                | Some n    -> if MapName.mem n ce.ce_arity
                               then ( ce.ce_nextsupid <- succ ce.ce_nextsupid ; NameInt ce.ce_nextsupid )
                               else n
            )
  and arity = List.length args
  and nbody = superAlpha args (superNormalize body)
  in ( ce.ce_supers <- MapName.add sid nbody ce.ce_supers  ;
       ce.ce_arity  <- MapName.add sid arity ce.ce_arity ;
       creturn sid
     ) )


(*****************************
 *                           *
 *      Lambda Lifting       *
 *                           *
 *****************************)

type liftEnv = { le_env     : name atom MapName.t ;
                 le_args    : name list           ;
                 le_apps    : name super list     ;
                 le_stack   : name super list     ;
                 le_name    : name option         ;
               }

let liftEnv a b c d e = { le_env     = a ;
                          le_args    = b ;
                          le_apps    = c ;
                          le_stack   = d ;
                          le_name    = e
                        }
(** Empty lambda lifting environment *)
let liftEnvZero = liftEnv MapName.empty [] [] [] None

(** keep the env but set everyting else to default *)
let leKeepEnv lenv = liftEnv lenv.le_env [] [] [] None

(** Are we still in the apply fecthing phase *)
let isFetchApps lenv = ( lenv.le_args = [] )

(** set the name *)
let leSetName x le = { le with le_name = Some x }

(** add an apply to the environment *)
let leAddApp x le = match le.le_args with
 | [] -> { le with le_apps = x :: le.le_apps }
 | _  -> failwith "Can not add apps to lift env when agrs != []"


let leAddApps l le = List.fold_left (swap leAddApp) le l

(** add an argument to the environment *)
let leAddArg x le = match le.le_apps with
 | []     -> creturn ( { le with le_args    = x :: le.le_args
                       }
                     )
 | y :: q -> (match superIsValue y with
                | Some z -> let tryRegZ = match ( z , x ) with
                                            | ( Sup ( NameInt i ) , NameStr n ) -> (trySetName i n >> creturn ())
                                            | _                                 -> creturn  ()
                            in tryRegZ >>
                               creturn ( { le with le_env     = MapName.add x z le.le_env ;
                                                   le_apps    = q
                                         }
                                       )
                | None   -> creturn ( { le with le_stack   = y :: le.le_stack ;
                                                le_apps    = q ;
                                                le_args    = x :: le.le_args 
                                      }
                                    )
             )


let leAddArgs l e = cfoldlM (swap leAddArg) e l


(** subsitute an atom by the environment *)
let leSubst lenv = function
 | Var v -> if MapName.mem v lenv.le_env
            then MapName.find v lenv.le_env
            else Var v
 | a     -> a

(** add/remove a new (variable , atom) to the environment *)
let leEnvAdd x t lenv = { lenv with le_env = MapName.add    x t lenv.le_env }
let leEnvRem x   lenv = { lenv with le_env = MapName.remove x   lenv.le_env }

(** register a super from the environment *)
let leRebuild lenv u =
 let listapp = (List.rev lenv.le_stack) @ lenv.le_apps
 in match List.rev lenv.le_args with
      | [] -> creturn (SApp (u , listapp))
      | l  -> let fv      = superFreeVars u                           in
              let fvreal  = List.fold_left (swap SetName.remove) fv l in
              let fvreall = SetName.elements fvreal                   in
              registerSuper (fvreall @ l) u lenv.le_name >>= (fun sid ->
              creturn (SApp ( SAtm ( Sup ( sid ) )
                            , (List.map mkSuperVar fvreall) @ listapp
                            )
                      )
              )

let debug s t =
 print_string ("\n\n=================   " ^ s ^ "  ================\n\n") ;
 print_string (Pretty.pretty_run (pretty_core t)) ;
 print_string ("\n\n======================================" ^ (String.make (String.length s) '=') ^ "\n\n") ;
 flush stdout


(** transforms a core into a super

    !!!!!  /!\   The core MUST BE alpha renamed  !!!!!
    !!!!! / ! \                                  !!!!!
  *)
let lambdaLift e =
 let rec aux lenv u = match coreTerm u with
   | App (s,t)  -> if isFetchApps lenv
                   then aux (leKeepEnv lenv) t >>= (fun t' -> aux (leAddApp t' lenv) s)
                   else aux (leKeepEnv lenv) u >>= (fun u' -> leRebuild lenv u'       )

   | Fun (x,t)  -> leAddArg x (leEnvRem x lenv) >>= (fun nenv ->  aux nenv t)

   | Rec (x,t)  -> (* alpha : alenv = the env but x *)
                   let alenv = leEnvRem x lenv                                                in
                   let fvt   = freeVar t                                                      in
                   let fv    = SetName.filter (fun x -> not (MapName.mem x alenv.le_env)) fvt in                        
                   if not (SetName.mem x fvt)
                   then aux alenv t
                   else (* we have to deal with recursion *)
                        (* free vars but not the one defined in the env *)
                        let fvl  = SetName.elements    (SetName.remove x fv)  in
                        let fvv  = List.map mkVar      fvl                    in
                        let fvsv = List.map mkSuperVar fvl                    in
                        (* we will substitute x by x fv0 .... fvn so fvi must not be bound !!! *)
                        coreAlphaWith fvt t >>= (fun u   ->
                        getNewSuperId       >>= (fun sid ->
                        let u' = substitute (substAdd sid (multiApp (mkSup sid) fvv) substEmpty) u in
                        if isFetchApps alenv
                        then (* still in apps fetch mode, we can continue *)
                             leAddArgs fvl (leSetName sid (leAddApps (List.rev fvsv)            alenv ))
                             >>= (fun newenv -> aux newenv u')
                        else (* we are in args fetch mode, we need to separate the rec from the rest*)
                             leAddArgs fvl (leSetName sid (leAddApps (List.rev fvsv) (leKeepEnv alenv)))
                             >>= (fun newenv -> aux newenv u' >>= leRebuild lenv)
                        ))

   | Atm a      -> leRebuild lenv (SAtm (leSubst lenv a))

   | Seq (l,c)  -> let f = aux (leKeepEnv lenv)
                   in cmapM f l >>= (fun l' ->
                      f c       >>= (fun c' ->
                      leRebuild lenv (SSeq (l' @ [ c' ])) ))

   | If (c,t,e) -> let newenv = leKeepEnv lenv in
                   aux newenv c >>= (fun c' ->
                   aux newenv t >>= (fun t' ->
                   aux newenv e >>= (fun e' ->
                   leRebuild lenv (SIf (c',t',e')) )))
 in aux liftEnvZero e


(** compiles a core to super combinators *)
let compileCore e =
 lambdaLift e             >>= (fun e'  ->       
 registerSuper [] e' None >>= (fun sid ->
 match sid with
  | NameStr _ -> creturn sid
  | NameInt i -> trySetName i "main"
 ))
 


(*****************************
 *                           *
 *      Code Generation      *
 *                           *
 *****************************)



(** outputs super combinators *)
let superCombinators = CompM (fun c ->
  let substSuperId  = function
   | NameInt i as s  -> if MapInt.mem i c.ce_names
                         then NameStr (MapInt.find i c.ce_names)
                         else s
   | s                -> s
  in let substAtom  = function
   | Sup s            -> Sup (substSuperId s)
   | a                -> a
  in let substSuper key sup l = ( entry (substSuperId key)
                                        (MapName.find key c.ce_arity)
                                        (superMap substAtom sup)
                                ) :: l
  in List.rev (MapName.fold substSuper c.ce_supers [])
)



(** pushes a statement on the code field *)
let pushCode x = CompM (fun c -> c.ce_code <- x :: c.ce_code)

(** reset the code stack *)
let codeReset = CompM (fun c -> c.ce_code <- [])

(** get the code stack *)
let codeGet   = CompM (fun c -> let x = List.rev c.ce_code
                                in ( c.ce_code <- [] ; x )
                      )

(** locality for codes *)
let codeLocal (CompM m) = CompM (fun c ->
  let codes = c.ce_code
  in  let r = m c
      in  ( c.ce_code <- codes ;
            r
          ) )

(** compiles a super combinator IN NORMALIZED FORM into code
    Arguments are evaluated right to left so that the first argument
    comes first out of the stack when evaluating closure
 *)

let compileApply = function
    | None   -> pushCode ( CPop  1 ) >> (* TMP1 = arg     *)
                pushCode ( CApply (ATmp 1))
    | Some a -> pushCode ( CApply    a    )

let compileCallArg (i,l) = function
  | None   -> pushCode (CPop i) >>
              creturn ((i + 1) , (ATmp i) :: l)
  | Some a -> creturn (i , a :: l)


let compileSuper z =
  let rec compile cl = function (* cl = argument list *)
     | SApp (s , l)   -> cmapM compileArg (List.rev l) >>= (fun cl2 ->
                         compile ((List.rev cl2) @ cl) s )

     | SSeq         l -> let (l', c) = findLast l
                         in citerM (compile []) l' >> compile cl c

     | SAtm (Sup sid) -> getArity sid >>= (fun oar -> match oar with
                          | None    -> failwith ("compile: " ^ (stringOfName sid) ^ " is unknown")
                          | Some ar -> let n = List.length cl
                                       in if ar > n
                                       then pushCode (CMkClosure (ar,sid)) >>
                                            citerM compileApply cl    >>
                                            creturn ()
                                       else let l1 = take ar cl
                                            and l2 = drop ar cl
                                            in (* compiling the call *)
                                               cfoldlM compileCallArg (1,[]) l1 >>= (fun (_,arglrev) ->
                                               (* arglrev is reversed *)
                                               pushCode (CCall (sid , List.rev arglrev)) >>
                                               (* applying the rest of arguments *)
                                               citerM compileApply          l2 >>
                                               creturn () )
                         )

     | SAtm a         -> pushCode ( CArg (argOfAtom a) )    >>
                         citerM compileApply cl             >>
                         creturn ()

     | SIf (c,t,e)    -> compile [] c >>
                         codeLocal ( codeReset    >>
                                     compile cl t >>
                                     codeGet
                                   )  >>= (fun t' ->
                         codeLocal ( codeReset    >>
                                     compile cl e >>
                                     codeGet
                                   )  >>= (fun e' ->
                         pushCode ( CIf (t',e')) ))


                    
  and compileArg = function
     | SAtm ( Sup sid ) -> getArity sid >>= (fun oar ->
                           match oar with
                            | None    -> failwith ("compileArg: " ^ (stringOfName sid) ^ " is unknow")
                            | Some ar -> pushCode (CMkClosure (ar,sid)) >>
                                         pushCode  CPush                >>
                                         creturn None
                           )
     | SAtm   a         -> creturn (Some (argOfAtom a))
     | t                -> compile [] t  >>
                           pushCode CPush >>
                           creturn  None

 in ( codeReset    >>
      compile [] z >>
      codeGet
    )


(** code optimisation *)
let rec optimizeCode = function
 | []                    -> []
 | CPush :: CPop i  :: l -> optimizeCode (CSave i :: l)
 | CPush :: CSave i :: l -> CSave i :: (optimizeCode (CPush :: l))
 | CIf (t,e) :: l        -> CIf ( optimizeCode t
                                , optimizeCode e
                                ) :: (optimizeCode l)
 | c :: l                -> c :: (optimizeCode l)


(** monadic map en entry *)
let entryMapM f e = f e.e_body >>= (fun s -> creturn { e with e_body = s } )

(** outputs the list of code entries *)
let codes    = superCombinators >>= cmapM (entryMapM compileSuper)

(** optimized version *)
let compileSuperOpt x = compileSuper x >>= (fun c -> creturn (optimizeCode c))
let codesOpt          = superCombinators >>= cmapM (entryMapM compileSuperOpt)


