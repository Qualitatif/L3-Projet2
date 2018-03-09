open Expr

let rec eval_var var env =
  match env with
  | [] -> failwith "Error: variable not declared"
  | (str,value)::r -> if str==var then value else eval_var var r
  
let rec eval_anon e env =
  match e with
  | Const(k) -> env
  | Var(str) -> env
  | Add(e1,e2) -> env
  | Mul(e1,e2) -> env
  | Sub(e1,e2) -> env
  | Let(var,e1,e2) -> env
  | Let_anon(e1,e2) -> env
  | Ite(c,e1,e2) -> env
  | PrInt(e) -> env

let rec eval e env =
  match e with
  | Const(k) -> k
  | Var(str) -> eval_var str env
  | Add(e1,e2) -> (eval e1 env) + (eval e2 env)
  | Mul(e1,e2) -> (eval e1 env) * (eval e2 env)
  | Sub(e1,e2) -> (eval e1 env) - (eval e2 env)
  | Let(var,e1,e2) -> (match var with
  					  |Var(str) -> eval e2 ((str,eval e1 env)::env)  
  					  |_ -> failwith "Error: Incorrect name of variable")
  | Let_anon(e1,e2) -> eval e2 (eval_anon e1 env)
  | Ite(c,e1,e2) -> if eval_cond c env then eval e1 env else eval e2 env
  | PrInt(e) -> let _ = print_int(eval e env) in eval e env  
  
and eval_cond c env =
  match c with
  | True -> true
  | False -> false
  | Le(e1,e2) -> (eval e1 env) <= (eval e2 env)
  | Lt(e1,e2) -> (eval e1 env) < (eval e2 env)
  | Ge(e1,e2) -> (eval e1 env) >= (eval e2 env)
  | Gt(e1,e2) -> (eval e1 env) > (eval e2 env)
  | Eq(e1,e2) -> (eval e1 env) == (eval e2 env)
  | Neq(e1,e2) -> (eval e1 env) != (eval e2 env)
  | And(c1,c2) -> (eval_cond c1 env) && (eval_cond c2 env)
  | Or(c1,c2) -> (eval_cond c1 env) || (eval_cond c2 env)
  | Not(c) -> not (eval_cond c env)
