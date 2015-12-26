exception Typecheck_failed of string
exception Desugar_failed
exception Self_referential

type resultS = NumS of float
             | BoolS of bool
             | AopS of char * resultS * resultS
             | BopS of string * resultS * resultS
             | CopS of string * resultS * resultS
             | FunS of string option * string list * resultS
             | VarS of string
             | AppS of resultS * resultS
             | LetS of string * resultS * resultS
             | ParS of resultS * resultS
             | IfS of resultS * resultS * resultS
             | ListS of resultS list
             | FstS of resultS
             | SndS of resultS
             | HeadS of resultS
             | TailS of resultS
             | ConsS of resultS * resultS
             | EqualS of resultS * resultS
             | NotS of resultS

type resultC = NumC of float
             | BoolC of bool
             | AopC of char * resultC * resultC
             | BopC of string * resultC * resultC
             | CopC of string * resultC * resultC
             | FunC of string option * string * resultC
             | VarC of string
             | AppC of resultC * resultC
             | LetC of string * resultC * resultC
             | ParC of resultC * resultC
             | IfC of resultC * resultC * resultC
             | ListC of resultC list
             | FstC of resultC
             | SndC of resultC
             | HeadC of resultC
             | TailC of resultC
             | ConsC of resultC * resultC
             | EqualC of resultC * resultC
             | NotC of resultC

type 'a env = (string * 'a) list

type value = Num of float
           | Bool of bool
           | Closure of value env * resultC
           | Delay of value option ref
           | Pair of value * value
           | List of value list
           | Error of string

type ty = NumT
        | BoolT
        | ArrowT of ty * ty
        | VarT of int
        | ParT of ty * ty
        | ListT of ty
        | PolyT of ty list * ty

type exprCT = resultC * ty

let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b          -> string_of_bool b
  | Closure (e, x)  -> "Closure"
  | Delay r         -> "Delayed value"
  | Pair (l,r)      -> "("^(valToString l)^","^(valToString r)^")" 
  | List xs         -> (match xs with
                        | []      -> "[]"
                        | x::[]   -> "["^(valToString x)^"]"
                        | xs      -> let xs' = List.rev xs 
                                     in "["^(List.fold_right 
                                              (fun x acc -> (valToString x)^","^acc)
                                              (List.tl xs') (valToString @@ List.hd xs'))
                                        ^"]")
  | Error s         -> "Error In Value: "^s

let rec tyToString r = match r with
  | NumT          -> "Num"
  | BoolT         -> "Bool"
  | ArrowT (a,b)  -> "("^(tyToString a)^" -> "^(tyToString b)^")"
  | VarT i        -> "a"^(string_of_int i)
  | ParT (l,r)    -> "("^(tyToString l)^","^(tyToString r)^")"
  | ListT t       -> "["^(tyToString t)^"]"
  | PolyT (_,_)   -> "Polymorphic Type"

(* freeVars : string list -> resultC -> string list *)
let rec freeVars env r = match r with
  | VarC s        -> if List.exists (fun x -> x==s) env then [s] else env
  | AopC (c,l,r)  -> freeVars env l @ freeVars env r
  | BopC (s,l,r)  -> freeVars env l @ freeVars env r
  | CopC (s,l,r)  -> freeVars env l @ freeVars env r
  | FunC (n,p,f)  -> (match n with
                      | Some fName  -> freeVars (fName :: p :: env) f
                      | None        -> freeVars (p :: env) f)
  | AppC (f,p)    -> freeVars env f @ freeVars env p
  | ParC (l,r)    -> freeVars env l @ freeVars env r
  | IfC (b,t,e)   -> freeVars env b @ freeVars env t @ freeVars env e
  | ListC xs      -> List.concat @@ List.map (freeVars env) xs
  | FstC x  
  | SndC x
  | HeadC x
  | TailC x       -> freeVars env x
  | ConsC (x,xs)  -> freeVars env x @ freeVars env xs
  | EqualC (l,r)  -> freeVars env l @ freeVars env r
  | _             -> []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if (compare s str)==0 then Some v else lookup str tl

(*desugar : resultS -> resultC *)
let rec desugar expr = match expr with
  | NumS i        -> NumC i
  | BoolS b       -> BoolC b
  | AopS (c,l,r)  -> AopC (c,desugar l,desugar r)
  | BopS (s,l,r)  -> BopC (s,desugar l,desugar r)
  | CopS (s,l,r)  -> CopC (s,desugar l,desugar r)
  | FunS (n,ss,r) -> (match List.fold_right (fun x acc -> FunC (None,x,acc))  
                                           ss 
                                           (desugar r) 
                     with
                      | FunC (_,s',f')  -> FunC (n,s',f')
                      | _               -> raise Desugar_failed)
  | VarS s        -> VarC s
  | AppS (f,p)    -> AppC (desugar f, desugar p)
  | LetS (s,x,y)  -> LetC (s,desugar x,desugar y)
  | ParS (l,r)    -> ParC (desugar l,desugar r)
  | IfS (b,t,e)   -> IfC (desugar b,desugar t,desugar e)
  | ListS xs      -> ListC (List.map desugar xs)
  | FstS x        -> FstC (desugar x)
  | SndS x        -> SndC (desugar x)
  | HeadS x       -> HeadC (desugar x)
  | TailS x       -> TailC (desugar x)
  | ConsS (l,r)   -> ConsC (desugar l,desugar r)
  | EqualS (l,r)  -> EqualC (desugar l,desugar r)
  | NotS x        -> NotC (desugar x)

(* envToString : value env -> string *)
let rec envToString env = match env with
  | []      -> "[]"
  | x::xs   -> "("^(fst x)^", "^(valToString (snd x))^") :: "^(envToString xs)

(* isEqual : val -> val -> boolean *)
let rec isEqual l r = match (l,r) with
  | (Num x,Num y)               -> x=y
  | (Bool x,Bool y)             -> x=y
  | (Pair (x,y),Pair (a,b))     -> (isEqual x a)&&(isEqual y b)
  | (List [],List [])           -> true
  | (List (x::xs),List (y::ys)) -> (isEqual x y)&&(isEqual (List xs) (List ys))
  | _                           -> false

(* interp : Value env -> resultC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC b       -> Bool b
                   (* I'm assuming that OCAML will only check snd if fst succeeds *)
  | AopC (c,l,r)  -> (match (c,interp env l,interp env r) with
                      | ('+',Num lv,Num rv) -> Num (lv +. rv)
                      | ('*',Num lv,Num rv) -> Num (lv *. rv)
                      | ('-',Num lv,Num rv) -> Num (lv -. rv)
                      | ('/',Num lv,Num rv) -> if rv=0.
                                               then Error "Divizion by zero"
                                               else Num (lv /. rv)
                      | (x,Num lv,Num rv)   -> Error "Invalid operation"
                      | _                   -> Error "Invalid arithmetic statement")
  | BopC (s,l,r)  -> (match (s,interp env l,interp env r) with
                      | ("&&",Bool lv,Bool rv)  -> Bool (lv&&rv)
                      | ("||",Bool lv,Bool rv)  -> Bool (lv||rv)
                      | (x,Bool lv,Bool rv)     -> Error ("Invalid operation "^x)
                      | _                       -> Error "Invalid boolean statement")
  | CopC (s,l,r)  -> (match (s,interp env l,interp env r) with
                      | (">",Num lv,Num rv)   -> Bool (lv>rv)
                      | ("<",Num lv,Num rv)   -> Bool (lv<rv)
                      | ("=>",Num lv,Num rv)
                      | (">=",Num lv,Num rv)  -> Bool (lv>=rv)
                      | ("<=",Num lv,Num rv)
                      | ("=<",Num lv,Num rv)  -> Bool (lv<=rv)
                      | (x,Num lv,Num rv)     -> Error ("Invalid operation "^x)
                      | _                     -> Error "Invalid comparison operation")
  | FunC (n,p,f)  -> Closure (env,r)
                    (*let fv = freeVars (List.map fst env) r in Closure (env, r)
                    let env2 = (* filter (\b -> (fst b) `elem` fv) *) env in *)
  | VarC s        -> (match lookup s env with
                      | Some (Delay r)  -> (match !r with
                                            | Some v  -> v
                                            | None    -> Error "No resolution")
                      | Some x          -> x
                      | None            -> Error ("Variable "^s^" not found in "
                                            ^"environment "^(envToString env)))
  | AppC (f,p)    -> let (fv,pv) = (interp env f, interp env p) in
                      (match fv with
                      | Closure (e2,FunC (Some n,fp,ff)) -> interp ((n,fv)::(fp,pv)::e2) ff 
                      | Closure (e2,FunC (None,fp,ff))   -> interp ((fp,pv)::e2) ff
                      | Error x                          -> Error x
                      | x                                -> Error "Not given a closure.")
  | LetC (s,x,y)  -> let r = ref None in
                     let env' = (s,Delay r)::env in
                     let fv = interp env' x in
                     (r := Some fv; interp env' y)
  | ParC (l,r)    -> Pair (interp env l,interp env r)
  | IfC (b,t,e)   -> (match interp env b with
                      | Bool bv -> if bv then interp env t else interp env e
                      | _       -> Error "If statement not given a boolean expression")
  | ListC xs      -> List (List.map (interp env) xs)
  | FstC x        -> (match interp env x with
                      | Pair (l,r)  -> l
                      | _           -> Error "fst not given a pair")
  | SndC x        -> (match interp env x with
                      | Pair (l,r)  -> r
                      | _           -> Error "snd not given a pair")
  | HeadC x       -> (match interp env x with
                      | List []     -> Error "head given an empty list"
                      | List (x::_) -> x
                      | _           -> Error "head not given a list")
  | TailC x       -> (match interp env x with
                      | List []     -> List []
                      | List (_::x) -> List x
                      | _           -> Error "tail not given a list")
  | ConsC (x,xs)  -> (match (interp env x,interp env xs) with
                      | (v,List vs) -> List (v::vs)
                      | _           -> Error "cons given invalid input")
  | EqualC (l,r)  -> (match (interp env l,interp env r) with
                      | (Error s,_)
                      | (_,Error s) -> Error s
                      | (x,y)       -> Bool (isEqual x y))
  | NotC x        -> (match interp env x with
                      | Bool b      -> Bool (not b)
                      | Error s     -> Error s
                      | _           -> Error "not not given bool")

let newc = let count = ref 0
           in (fun x -> (count := !count+1; VarT !count))

(* getUnresolved : ty -> ty list *)
let rec getUnresolved ty = match ty with
  | VarT i        -> ty::[]
  | ArrowT (a,b)  -> (getUnresolved a) @ (getUnresolved b)
  | ParT (l,r)    -> (getUnresolved l) @ (getUnresolved r)
  | ListT xs      -> getUnresolved xs
  | PolyT (is,t)  -> is
  | _             -> []

(* ty list -> ty -> ty list *)
let rec getVars ty = match ty with
  | VarT i        -> [VarT i]
  | ArrowT (a,b)  -> (getVars a) @ (getVars b)
  | ParT (l,r)    -> (getVars l) @ (getVars r)
  | ListT xs      -> getVars xs
  | _             -> []

(* extendNG : ty list -> ty list -> ty list *)
let addNonDup l1 l2 = (List.filter (fun x -> not (List.mem x l2)) l1) @ l2

(* extendGenerics' : ty list -> (ty * ty) list -> ty list *)
(* If you see a non-generic on either side of the constraint, make all the variables
   on the other side non-generic, then do a pass through the whole list again *)
let rec extendGenerics' ng cons = match cons with
  | []                      -> ng
  | (NumT,x)::tl 
  | (x,NumT)::tl
  | (BoolT, x)::tl 
  | (x,BoolT)::tl           -> extendGenerics' (addNonDup (getVars x) ng) tl
  | (VarT i,VarT j)::tl     -> if List.mem (VarT i) ng
                               then extendGenerics' (addNonDup [VarT j] ng) tl
                               else if List.mem (VarT j) ng
                                    then extendGenerics' ((VarT i)::ng) tl
                                    else extendGenerics' ng tl
  | (VarT i,x)::tl
  | (x,VarT i)::tl          -> if List.mem (VarT i) ng 
                               then extendGenerics' (addNonDup (getVars x) ng) tl
                               else extendGenerics' ng tl
  | _::tl                   -> extendGenerics' ng tl

(* extendGenerics : ty list -> (ty * ty) list -> ty list *)
let rec extendGenerics ng cons = let ng' = extendGenerics' ng cons 
                                 in if (List.length ng)==(List.length ng') 
                                    then ng' else extendGenerics ng' cons

(* resolve : int -> int -> ty -> ty *)
let rec resolve newI oldI ty = match ty with
  | VarT i        -> VarT (if i==oldI then newI else i)
  | ParT (l,r)    -> ParT (resolve newI oldI l,resolve newI oldI r)
  | ArrowT (a,b)  -> ArrowT (resolve newI oldI a,resolve newI oldI b)
  | ListT xs      -> ListT (resolve newI oldI xs)
  | _             -> ty

let resolveVars n o ty = match (n,o) with
  | (VarT newI,VarT oldI) -> resolve newI oldI ty
  | _                     -> raise (Typecheck_failed "resolveVars not given variables")

(* handlePoly : ty -> ty *)
let rec handlePoly ty = match getUnresolved ty with
  | []  -> ty
  | is  -> PolyT (is,ty)

(* constrgen : ty list -> ty env -> resultC -> ((ty * ty) list) * ty *)
let rec constrgen' ng env expr = match expr with
  | NumC i            -> ([],NumT)
  | BoolC b           -> ([],BoolT)
  | AopC (c,l,r)      -> let (l1,t1) = constrgen' ng env l
                         in let (l2,t2) = constrgen' ng env r
                         in (((t1,NumT) :: (t2,NumT) :: (List.append l1 l2)),NumT)
  | BopC (s,l,r)      -> let (l1,t1) = constrgen' ng env l 
                         in let (l2,t2) = constrgen' ng env r
                         in (((t1,BoolT) :: (t2,BoolT) :: (List.append l1 l2)),BoolT)
  | CopC (s,l,r)      -> let (l1,t1) = constrgen' ng env l 
                         in let (l2,t2) = constrgen' ng env r
                         in (((t1,NumT) :: (t2,NumT) :: (List.append l1 l2)),BoolT)
  | VarC s            -> (match lookup s env with
                          | Some (PolyT (ts,t)) -> 
                              let nt = List.fold_left 
                                   (fun acc v -> let nt = newc () 
                                                 in resolveVars nt v acc) 
                                   t ts
                              in ([],nt)
                          | Some t  -> ([],t)
                          | None    -> raise (Typecheck_failed ("Cannot find "^s)))
  | FunC (None,p,f)   -> let nt = newc () 
                         in let (l1,t1) = constrgen' (nt::ng) ((p,nt)::env) f 
                         in (l1,ArrowT (nt,t1))
  | FunC (Some n,p,f) -> let nt1 = newc () in let nt2 = newc () 
                         in let (l1,t1) = 
                              constrgen' (nt1::nt2::ng) ((p,nt1)::(n,nt2)::env) f 
                         in (l1,ArrowT (nt1,t1)) 
  | AppC (f,p)        -> let (l1,t1) = constrgen' ng env f 
                         in let (l2,t2) = constrgen' ng env p 
                         in let nt = newc () 
                         in ((t1,ArrowT(t2,nt))::(List.append l1 l2),nt)
  | LetC (s,l,r)      -> let nt = newc () 
                         in let (l1,t1') = constrgen' (nt::ng) ((s,nt)::env) l 
                         in let unr = getUnresolved t1'
                         in let ext = extendGenerics (nt::ng) l1
                         in let gens = List.filter (fun x -> not @@ List.mem x ext) unr
                         in let t1 = PolyT (gens,t1')
                         in let (l2,t2) = constrgen' ng ((s,t1)::env) r
                         in ((nt,t1')::(List.append l1 l2),t2)
  | ParC (l,r)        -> let (l1,t1) = constrgen' ng env l 
                         in let (l2,t2) = constrgen' ng env r 
                         in (List.append l1 l2,ParT (t1,t2))
  | IfC (b,t,e)       -> let (l1,t1) = constrgen' ng env b  
                         in let (l2,t2) = constrgen' ng env t 
                         in let (l3,t3) = constrgen' ng env e 
                         in ((t1,BoolT)::(t2,t3)::(List.append l1 (List.append l2 l3)),t3)
  | ListC []          -> let nt = newc () in ([],ListT nt)
  | ListC (x::xs)     -> let (l1,t1) = constrgen' ng env x 
                         in let ls = List.map (fun x -> let (l,t) = constrgen' ng env x 
                                                        in (t1,t)::l) xs
                         in (List.append l1 (List.concat ls),ListT t1)
  | FstC x            -> let nt1 = newc ()
                         in let nt2 = newc ()
                         in let (l1,t1) = constrgen' ng env x
                         in ((t1,ParT (nt1,nt2))::l1,nt1)
  | SndC x            -> let nt1 = newc ()
                         in let nt2 = newc ()
                         in let (l1,t1) = constrgen' ng env x
                         in ((t1,ParT (nt1,nt2))::l1,nt2)
  | HeadC xs          -> let nt = newc ()
                         in let (l1,t1) = constrgen' ng env xs
                         in ((t1,ListT nt)::l1,nt)
  | TailC xs          -> let nt = newc ()
                         in let (l1,t1) = constrgen' ng env xs
                         in ((t1,ListT nt)::l1,ListT nt)
  | ConsC (x,xs)      -> let (l1,t1) = constrgen' ng env x
                         in let (l2,t2) = constrgen' ng env xs
                         in ((t2,ListT t1)::(List.append l1 l2),t2)
  | EqualC (l,r)      -> let (l1,t1) = constrgen' ng env l
                         in let (l2,t2) = constrgen' ng env r
                         in ((t1,t2)::(List.append l1 l2),BoolT)
  | NotC b            -> let (l1,t1) = constrgen' ng env b
                         in ((t1,BoolT)::l1,BoolT)

(* constrgen : resultC -> (ty * ty) list * ty  *)
let constrgen expr = constrgen' [] [] expr

(* contains : int -> ty -> bool *)
let rec contains i t = match t with
  | ArrowT (l,r)
  | ParT (l,r)    -> (contains i l)||(contains i r)
  | VarT j        -> i==j
  | NumT | BoolT  -> false
  | ListT t       -> contains i t
  | PolyT (is,ty) -> contains i ty

(* subst1 : int -> ty -> ty -> ty *)
let rec subst1 i x t = match t with
  | VarT j        -> if j==i then x else t
  | ArrowT (a,b)  -> ArrowT (subst1 i x a,subst1 i x b)
  | ParT (l,r)    -> ParT (subst1 i x l,subst1 i x r)
  | _             -> t

(* subst2 : int -> ty -> (ty * ty) -> (ty * ty) *)
let subst2 i x (a,b) = (subst1 i x a,subst1 i x b)

let substList i x ls = List.map (subst2 i x) ls

(* unify : (ty * ty) list -> (ty * ty) list *)
let rec unify ls = match ls with
  | []                                -> []
  | (NumT,NumT)::tl                   -> unify tl
  | (BoolT,BoolT)::tl                 -> unify tl
  | (ArrowT (l,r),ArrowT (x,y))::tl
  | (ParT (l,r),ParT (x,y))::tl       -> unify ((l,x) :: (r,y) :: tl)
  | (ListT xs,ListT ys)::tl           -> unify ((xs,ys) :: tl)
  | (VarT i,VarT j)::tl               -> let y = VarT j in  
                                         if i==j 
                                         then unify tl
                                         else (VarT i,y) :: unify (substList i y tl)
  | (VarT i,x)::tl | (x,VarT i)::tl   -> if contains i x 
                                         then raise Self_referential
                                         else (VarT i,x) :: unify (substList i x tl)
  | (x,y)::_                          -> output_string stdout (tyToString x); 
                                         output_string stdout " ?= ";
                                         output_string stdout (tyToString y); 
                                         raise (Typecheck_failed "Bad constraints")

(* typecheck' : (ty * ty) list -> ty -> ty *)
let rec typecheck' ts ty = match ty with
  | NumT          -> NumT
  | BoolT         -> BoolT
  | ArrowT (a,b)  -> ArrowT (typecheck' ts a,typecheck' ts b)
  | ParT (l,r)    -> ParT (typecheck' ts l,typecheck' ts r)
  | VarT i        -> List.fold_left 
                      (fun acc (x,y) -> 
                        (match (x,y) with
                          | (VarT j,q)
                          | (q,VarT j)  -> if j==i then typecheck' ts q else acc
                          | _           -> acc)) ty ts
  | ListT t       -> ListT (typecheck' ts t) 
  | PolyT _       -> raise (Typecheck_failed "PolyT type not allowed")

(* typecheck : resultC -> ty *)
let typecheck r = let (cons,ty) = constrgen r in typecheck' (unify cons) ty

(* evaluate : resultS -> val *)
let evaluate exprS = let exprC = desugar exprS 
                     in let _ = typecheck exprC in interp [] exprC
