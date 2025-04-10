(* CSE322 Compiler Assignment 0 - Task 1 *)

exception NotImplemented

(* 1. Basic Recursion *)

(* sum n: calculate 1 + 2 + ... + n *)
let rec sum n = match n with
  | 0 -> 0
  | n -> n + sum (n-1)

(* fac n: calculate 1 * 2 * ... * n *)
let rec fac n = match n with
  | 0 -> 1
  | n -> n * fac (n - 1)

(* fib n: return the n-th fibonacci number *)
let rec fib n = match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

(* pow (x, y): calculate x to the power of y *)
let rec pow (x, y) = match y with
  | 0 -> 1
  | y -> x * pow (x, y - 1)

(* gcd (x, y): find the great common divisor of x and y *)
let rec gcd (x, y) = match y with
  | 0 -> x
  | _ -> gcd (y, x mod y)

(* palindrome s: return true if s is a palindrome *)
let rec palindrome s =
  let len = String.length s in
  let rec aux i =
    if i >= len / 2 then true
    else if String.get s i <> String.get s (len - i - 1) then false
    else aux (i + 1)
  in
  aux 0

(* 2. List *)

(* max l: return the maximum value in l *)
let rec max l = match l with
  | [] -> 0
  | [x] -> x
  | hd::tl -> if hd > max tl then hd else max tl

(* exist l x: check if x exists in l *)
let rec exist l x = match l with
  | [] -> false
  | hd::tl -> if hd = x then true else exist tl x

(* count l x: count the number of x in l *)
let rec count l x = match l with
  | [] -> 0
  | hd :: tl -> if hd = x then 1 + count tl x else count tl x

(* reverse l: return the reversed l *)
let rec reverse l = match l with
  | [] -> []
  | hd :: tl -> (reverse tl) @ [hd]

(* find l x: return the index of the first x in l
   -1 if x does not exist in l *)
let rec find l x = match l with
  | [] -> -1
  | hd :: tl -> if hd = x then 0 else 
      let result = find tl x in
      if result = -1 then -1 else result + 1

(* findr l x: return the index of the last x in l
   -1 if x does not exist in l *)
let rec findr l x = match l with
  | [] -> -1
  | hd :: tl -> 
      let result = findr tl x in
      if result = -1 then if hd = x then 0 else -1 else result + 1


