
# 📚Compiler Assignments Overview

This repository contains four successive assignments for the CSE322 Compiler course. Each assignment directory (`as0`, `as1`, `as2`, `as3`) can be built and run with `dune`.

---

## Table of Contents

- [Assignment 0](#assignment-0)
- [Assignment 1](#assignment-1)
- [Assignment 2](#assignment-2)
- [Assignment 3](#assignment-3)

---

## 🎯Assignment 0

Basic list and tree-processing functions.

### 🛠Build & Execute

```bash
cd as0
dune build
dune exec as0
```
### ✅Output:

```text
Testing Task1.fac
  Passed.

Testing Task1.fib
  Passed.

Testing Task1.pow
  Passed.

Testing Task1.gcd
  Passed.

Testing Task1.palindrom
  Passed.

Testing Task1.exist
  Passed.

Testing Task1.count
  Passed.

Testing Task1.reverse
  Passed.

Testing Task1.find
  Passed.

Testing Task1.findr
  Passed.

Testing Task2.exist
  Passed.

Testing Task2.count
  Passed.

Testing Task2.inorder
  Passed.

Testing Task2.depth
  Passed.

Testing Task2.max
  Passed.
```

### 🧹Wrap up

```bash
dune clean
```

---

## 🎯Assignment 1

### 🛠Build & Execute

```bash
cd ../as1
dune build
dune exec as1
```

```text
data/test.fun:1:1-3: FUN
data/test.fun:1:5-11: ID(addsome)
data/test.fun:1:13-13: LPAREN
data/test.fun:1:14-14: ID(x)
...
data/test.fun:14:21-24: ELSE
data/test.fun:14:26-26: NUM(1)
Lexing done.
```

```bash
dune clean
```

---

## Assignment 2

### 🛠Build

```bash
cd ../as2
dune build
```
### ⚠️ Warnings
When running the build you may see output like:
```bash
6 shift/reduce conflicts.
```
These messages come from Menhir (the OCaml parser generator) indicating grammar ambiguities. <ins>**They are expected in this assignment**</ins> and do not prevent your code from compiling or running correctly. You can safely ignore them, or—if you’re curious—inspect and refine parser.mly to eliminate the conflicts.

### ▶️ Execute

```bash
dune exec as2
```

### ✅Output

```text
Program:                            t
fun int_ops(x:int):int =
   let a = x + 5 * 3 in
   let b = a - -1 in
   let c = b - 16 in
     if if if c < 10 then 1 else not c < 10 then x = c else 0 then
       x
     else
       0
fun dec_ref(x:int ref):<> = let a = !x in
   let _ = x := a - 1 in
     <>
fun fact(x:int
ref):int =
   if (!x) < 1 then
     1
   else
     let a = !x in
     let b = dec_ref(x) in
       a * fact(x)
fun add_pair(x:<int, int>):int = let a = #0 x in
   let b = #1 x in
     a + b
fun add_pair_silly(x:<>):<int, int> -> int = add_pair
fun loop(x:int):int =
   let a = ref (x:int) in
   let _ = let _ = while not (!a) < 701 do a := (!a) - 1 in
     <> in
     !a
fun main(arg:int):int =
   let z = int_ops(add_pair_silly(<>)(<2, 4>)) in
   let a = fact(ref (z:int)) in
   let b = loop(a) in
   let _ = print_int(b) in
     b
Running Program...
700
Program Output:
700

Program:
fun addsome(x:int):int = x + 5
fun add(x:<int, int>):int = #0 x + #1 x
fun gt(t:<int, int>):int =
   let a = #0 t in
   let b = #1 t in
     not if a < b then not a = b else 0
fun main(argc:int):int =
   let x = (5 + 3) * 7 in
   let y = x + 7 in
     if gt(<x, 0>) then
       if not x = 7 then addsome(add(<x, y>)) else -1
     else
       if -2 then 0 else 1
Running Program...
Program Output:
124
```

### 🧹Wrap up

```bash
dune clean
```
---

## 🎯Assignment 3

### 🛠Build & Execute

```bash
cd as3
dune build
dune exec as3
```

### ✅Output
```text
Compiling data/all.fun ...
Program is successfully type-checked
Compiling data/test.fun ...
Program is successfully type-checked
```

### 🧹Wrap up
```bash
dune clean
```
