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
             | NullS of resultS
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
             | NullC of resultC
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

val printVal : out_channel -> value -> unit

val printTy : out_channel -> ty -> unit

val freeVars : string list -> resultC -> string list

val lookup : string -> 'a env -> 'a option

val desugar : resultS -> resultC

val toString : value -> string

val envToString : value env -> string

val interp : value env -> resultC -> value

val extendGenerics : ty list -> (ty * ty) list -> ty list

val resolveVars : ty -> ty -> ty -> ty

val constrgen' : ty list -> ty env -> resultC -> ((ty * ty) list) * ty

val constrgen : resultC -> (ty * ty) list * ty

val unify : (ty * ty) list -> (ty * ty) list

val typecheck' : (ty * ty) list -> ty -> ty

val typecheck : resultC -> ty

val getUnresolved : ty -> ty list

val resolve : int -> int -> ty -> ty

val handlePoly : ty -> ty

val evaluate : resultS -> value
