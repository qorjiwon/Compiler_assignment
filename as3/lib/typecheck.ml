(* CSE322 Compiler Assignment 3 *)
(* Type Checker *)

module A = Absyn

let rec list2string l = match l with
  | [] -> ""
  | [t] -> t
  | h::t -> h ^ "," ^ list2string t

let rec tp2string tp = match tp with
  | A.Inttp -> "int"
  | A.Tupletp tps -> "<" ^ (list2string (List.map tp2string tps)) ^ ">"
  | A.Arrowtp (tp1, tp2) -> tp2string tp1 ^ " -> " ^ tp2string tp2
  | A.Reftp tp -> tp2string tp ^ " ref"

type context = A.tp Symbol.table

exception Unimplemented

(* sub: check if t1 is a subtype of t2 *)
let rec sub (t1,t2) =
  let width(tl1, tl2) = if (List.length tl1) >= (List.length tl2) then true else false in
  let rec depth(tl1, tl2) = match tl2 with
    | tp2::tl2' -> (match tl1 with tp1::tl1'-> if sub(tp1,tp2) then depth(tl1',tl2') else false | _ -> false)
    | [] -> true
  in match t1 with
    | A.Inttp -> if t2 = A.Inttp then true else false
    | A.Tupletp tps1 -> (match t2 with
      | A.Tupletp tps2 -> if width(tps1, tps2) then depth (tps1, tps2) else false
      | _ -> false)
    | A.Arrowtp (t1p1, t1p2) -> (match t2 with
      | A.Arrowtp (t2p1, t2p2) -> if sub (t2p1,t1p1) && sub(t1p2,t2p2) then true else false
      | _ -> false)
    | A.Reftp tp1 -> (match t2 with
      | A.Reftp tp2 -> if tp1 = tp2 then true else false
      | _ -> false)

(* check_sub: raise error if t1 is not a subtype of t2 *)
let check_sub pos (tp1, tp2) =
   if sub (tp1, tp2) then ()
   else Errormsg.error (pos, (tp2string tp1)^" is not a subtype of "^(tp2string tp2))

(* complain: alias for Errormsg.error *)
let complain pos err = Errormsg.error (pos, err)

(* join: compute the join of t1 and t2 *)
let rec join pos (t1,t2) : A.tp =
  (* check if st1 and st2 can be joined or not *)
  let rec check_join (st1, st2) = match st1 with
    | A.Inttp -> if sub(st1,st2) then true else false
    | A.Tupletp tps1 -> (match st2 with
      | A.Tupletp tps2 -> true
      | _ -> false)
    | A.Arrowtp (t1p1, t1p2) -> (match st2 with
      | A.Arrowtp (t2p1, t2p2) -> check_join(t1p1,t2p1) && check_join(t1p2,t2p2)
      | _ -> false)
    | A.Reftp tp1 -> (match st2 with
      | A.Reftp tp2 -> if tp1=tp2 then true else false
      | _ -> false) in
  (* join tl1 list and tl2 list *)
  let rec rec_join (tl1, tl2) =
    match (tl1, tl2) with
    | (tp1::tl1', tp2::tl2') -> if check_join(tp1,tp2) then join pos (tp1,tp2)::(rec_join (tl1', tl2'))
                                else []
    | ([], _) -> []
    | (_, []) -> []
  in match t1 with
    | A.Inttp -> if sub(t1,t2) then A.Inttp
                 else (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1)
    | A.Tupletp tps1 -> (match t2 with
      | A.Tupletp tps2 -> A.Tupletp (rec_join (tps1, tps2))
      | _ -> (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))
    | A.Arrowtp (t1p1, t1p2) -> (match t2 with
      | A.Arrowtp (t2p1, t2p2) -> A.Arrowtp (join pos (t1p1,t2p1), join pos (t1p2,t2p2))
      | _ -> (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))
    | A.Reftp tp1 -> (match t2 with
      | A.Reftp tp2 -> if tp1 = tp2 then t1
                       else (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1)
      | _ -> (complain pos ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))

(* tc_exp: check the type of the given expression e *)
(* - ctxt: symbol table                             *)
(* - pos: position of the expression                *)
let rec tc_exp ctxt pos e : A.tp =
  match e with
  (* integer literal *)
  | A.Int _ ->
      A.Inttp

  (* variable lookup *)
  | A.Id x -> (
      match Symbol.find x ctxt with
      | Some t -> t
      | None ->
          complain pos ("Unbound variable " ^ Symbol.name x);
          A.Inttp
    )

  (* operators (unary/binary/ref ops) *)
  | A.Op (op, es) -> begin
      match op, es with
      (* binary int ops *)
      | A.Add, [e1; e2]
      | A.Sub, [e1; e2]
      | A.Mul, [e1; e2]
      | A.LT,  [e1; e2]
      | A.Eq,  [e1; e2] ->
          let t1 = tc_exp ctxt pos e1 in
          let t2 = tc_exp ctxt pos e2 in
          if not (sub (t1, A.Inttp) && sub (t2, A.Inttp)) then
            complain pos ("Operator " ^ tp2string A.Inttp ^ " requires int operands");
          A.Inttp

      (* reference creation *)
      | A.Ref, [e1] ->
          let t1 = tc_exp ctxt pos e1 in
          A.Reftp t1

      (* dereference *)
      | A.Get, [e1] ->
          let t1 = tc_exp ctxt pos e1 in
          begin match t1 with
          | A.Reftp inner -> inner
          | _ ->
              complain pos "Dereferencing non-reference";
              A.Inttp
          end

      (* assignment *)
      | A.Set, [e1; e2] ->
          let t1 = tc_exp ctxt pos e1 in
          let t2 = tc_exp ctxt pos e2 in
          begin match t1 with
          | A.Reftp inner ->
              if not (sub (t2, inner)) then
                complain pos "Assignment type mismatch";
              (* unit *)
              A.Tupletp []
          | _ ->
              complain pos "Assignment to non-reference";
              A.Tupletp []
          end

      (* ill-formed operator application *)
      | _ ->
          complain pos "Invalid operator or wrong number of operands";
          A.Inttp
    end

  (* tuple construction *)
  | A.Tuple es ->
      let ts = List.map (tc_exp ctxt pos) es in
      A.Tupletp ts

  (* tuple projection *)
  | A.Proj (i, e1) ->
      let t = tc_exp ctxt pos e1 in
      begin match t with
      | A.Tupletp ts ->
          if i < 0 || i >= List.length ts then (
            complain pos "Tuple index out of bounds";
            A.Inttp
          ) else
            List.nth ts i
      | _ ->
          complain pos "Projection from non-tuple";
          A.Inttp
      end

  (* conditional *)
  | A.If (cond, e_then, e_else) ->
      let tc = tc_exp ctxt pos cond in
      if not (sub (tc, A.Inttp)) then
        complain pos "Condition must be int";
      let t_then = tc_exp ctxt pos e_then in
      let t_else = tc_exp ctxt pos e_else in
      join pos (t_then, t_else)

  (* while *)
  | A.While (cond, body) ->
      let tc = tc_exp ctxt pos cond in
      if not (sub (tc, A.Inttp)) then
        complain pos "Condition must be int";
      let tb = tc_exp ctxt pos body in
      if not (sub (tb, A.Tupletp [])) then
        complain pos "Body must return unit";
      A.Tupletp []

  (* function call *)
  | A.Call (f, arg) ->
      let tf = tc_exp ctxt pos f in
      let ta = tc_exp ctxt pos arg in
      begin match tf with
      | A.Arrowtp (param, result) ->
          if not (sub (ta, param)) then
            complain pos "Function argument type mismatch";
          result
      | _ ->
          complain pos "Application to non-function";
          A.Inttp
      end

  (* let-binding *)
  | A.Let (x, e1, e2) ->
      let t1 = tc_exp ctxt pos e1 in
      let ctxt' = Symbol.add x t1 ctxt in
      tc_exp ctxt' pos e2

  (* explicit type constraint/subsumption *)
  | A.Constrain (e1, tp) ->
      let t1 = tc_exp ctxt pos e1 in
      check_sub pos (t1, tp);
      tp

  (* position annotation: just propagate new pos *)
  | A.Pos (pos', e1) ->
      tc_exp ctxt pos' e1


(* tc_fundec: check the type of the function definition *)
let tc_fundec ctxt ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
  let ctxt' = Symbol.add x tp1 ctxt in
  let tp = tc_exp ctxt' pos exp
  in check_sub pos (tp, tp2)

(* do_another_fun: update the types of the functions *)
let do_another_fun ctxt (pos, fdec) =
  let (f, x, tp1, tp2, exp) = fdec in
  match Symbol.find f ctxt with
    (* check if the function name is duplicated *)
    | Some x -> (Errormsg.error(pos,"function name (" ^ Symbol.name f ^ ") is duplicated"); ctxt)
    | None -> if (Symbol.name f) = "main" then (* check if main function has int->int type *)
                (if (tp1 = A.Inttp) && (tp2 = A.Inttp) then Symbol.add f (A.Arrowtp (tp1, tp2)) ctxt
                 else (Errormsg.error(pos,"main function has wrong type"); Symbol.add f (A.Arrowtp (tp1, tp2)) ctxt))
              else Symbol.add f (A.Arrowtp (tp1, tp2)) ctxt

(* build_global_context: generate the initial symbol table *)
let build_global_context (fundecs): context =
  List.fold_left do_another_fun (Symbol.add (Symbol.symbol "print_int") (A.Arrowtp (A.Inttp, A.Tupletp [])) Symbol.empty) fundecs

(* tc: check the type of the program *)
let tc (fundecs : A.prog)  =
  let ctxt = build_global_context(fundecs) in
  let _ = List.map (tc_fundec ctxt) fundecs in
  ()

