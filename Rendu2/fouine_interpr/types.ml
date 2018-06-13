(* We have two recursive types working together. *)

type expr =
    | Nil
    | Skip
    | Const of int
    | Var of string
    | PrInt of expr
    | Add of expr * expr
    | Mul of expr * expr
    | Sub of expr * expr
    | Div of expr * expr
    | Let of expr * expr * (expr option)
    | Ite of cond * expr * expr
    | Fun of expr * expr
    | App of expr * expr
    | Seq of expr * expr
    | Ref of expr
    | Bang of expr
    | Update of expr * expr
    | Pair of expr * expr
    | Fst of expr
    | Snd of expr
    | Excn of expr
    | Raise of expr
    | TryWith of expr * expr * expr

and cond =
    | True
    | False
    | Leq of expr * expr
    | Lt of expr * expr
    | Geq of expr * expr
    | Gt of expr * expr
    | Eq of expr * expr
    | Neq of expr * expr
    | And of cond * cond
    | Or of cond * cond
    | Not of cond

type closure = expr * expr * env
and env = (string * value) list
and value = I of int | Cl of closure | Id of int | P of value * value | E of int | C of bool | S
(* Ids are for references, cf: eval.ml. S is a fake value which only serves to turn unit into value in imperative programs *)

let rec set_f ar e = match ar with
    | [] -> e
    | v::a -> Fun (v, set_f a e)

let set_let (a,b) o = match a,b,o with
    | Pair (v1, v2), Pair (e1, e2), None    -> Seq (Let (v1, e1, None), Let (v2, e2, None))
    | Pair (v1, v2), Pair (e1, e2), Some e3 -> Let (v1, e1,
                                                    Some (Let (v2, e2, Some e3)))
    | Pair (v1, v2), e, None                -> Seq (Let (v1, Fst e, None),
                                                    Let (v2, Snd e, None))
    | Pair (v1, v2), e, Some e3             -> Let (v1, Fst e,
                                                    Some (Let (v2, Snd e,
                                                                Some e3)))
    | _                                     -> Let (a, b, o)
