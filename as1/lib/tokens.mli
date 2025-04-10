(* CSE322 Compiler Assignment 1 *)
(* Module that Defines Lexical Tokens *)

type ('svalue,'pos) token = string

val tok_arrow: Errormsg.pos * Errormsg.pos -> ('svalue,'pos) token
val tok_fun:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_in:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_let:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_else:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_then:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_if:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_assign:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_bang:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_ref:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_do:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_while:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_or:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_not:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_and:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_gt:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_eq:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_lt:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_proj: (Errormsg.pos * Errormsg.pos) * int ->  ('svalue,'pos) token
val tok_times:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_minus:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_plus:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_rparen:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_lparen:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_colon:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_semicolon:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_comma:  Errormsg.pos * Errormsg.pos ->  ('svalue,'pos) token
val tok_id: (Errormsg.pos * Errormsg.pos) * string ->  ('svalue,'pos) token
val tok_num: (Errormsg.pos * Errormsg.pos) * int ->  ('svalue,'pos) token
