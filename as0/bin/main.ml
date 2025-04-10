(* CSE322 Compiler Assignment 0 *)

open As0

module TestCase = struct
  let fac = [(1,1); (3,6); (5,120)]
  let fib = [(0,0); (1,1); (9,34)]
  let pow = [((5,0),1); ((2,2),4); ((3,3),27)]
  let gcd = [((2,3),1); ((24,4),4); ((15,6), 3)]
  let palindrome = [("aba",true); ("aa", true); ("abcde", false)]

  let list1 = []
  let list2 = [1;2;3;4;5]
  let list3 = [10;2;0;-3;0;8;0]

  let lexist = [(list1,0,false); (list2,3,true); (list3,0,true)]
  let lcount = [(list1,0,0); (list2,4,1); (list3,0,3)]
  let lrever = [(list1,[]); (list2,[5;4;3;2;1]); (list3,[0;8;0;-3;0;2;10])]
  let lfindf = [(list1,0,-1); (list2,3,2); (list3,0,2)]
  let lfindr = [(list1,0,-1); (list2,3,2); (list3,0,6)]

  let tree1 = Task2.Leaf 1
  let tree2 = Task2.Node (Task2.Leaf 1, 3, Task2.Leaf 10)
  let tree3 = Task2.Node (Task2.Node (Task2.Leaf 1, 2, Task2.Leaf 10), 3, Task2.Leaf 3)

  let texist = [(tree1,0,false); (tree2,3,true); (tree3,9,false)]
  let tcount = [(tree1,1,1); (tree2,7,0); (tree3,3,2)]
  let tinorder = [(tree1,[1]); (tree2,[1;3;10]); (tree3,[1;2;10;3;3])]
  let tdepth = [(tree1,0); (tree2,1); (tree3,2)]
  let tmax = [(tree1,1); (tree2,10); (tree3,10)]
end

module Tester = struct
  let test1 f (x, y) = if (f x) = y then true else false
  let test2 f (x1, x2, y) = if (f x1 x2) = y then true else false

  let runTest (s, f, cases) =
    let result = try print_endline ("Testing "^s^""); List.map f cases with
      | Task1.NotImplemented -> [false]
      | Task2.NotImplemented -> [false]
    in if List.fold_left (&&) true result then print_endline "  Passed.\n" else print_endline "  Failed.\n"

  let runAll() =
    runTest("Task1.fac", test1 Task1.fac, TestCase.fac);
    runTest("Task1.fib", test1 Task1.fib, TestCase.fib);
    runTest("Task1.pow", test1 Task1.pow, TestCase.pow);
    runTest("Task1.gcd", test1 Task1.gcd, TestCase.gcd);
    runTest("Task1.palindrom", test1 Task1.palindrome, TestCase.palindrome);
    runTest("Task1.exist", test2 Task1.exist, TestCase.lexist);
    runTest("Task1.count", test2 Task1.count, TestCase.lcount);
    runTest("Task1.reverse", test1 Task1.reverse, TestCase.lrever);
    runTest("Task1.find", test2 Task1.find, TestCase.lfindf);
    runTest("Task1.findr", test2 Task1.findr, TestCase.lfindr);
    runTest("Task2.exist", test2 Task2.exist, TestCase.texist);
    runTest("Task2.count", test2 Task2.count, TestCase.tcount);
    runTest("Task2.inorder", test1 Task2.inorder, TestCase.tinorder);
    runTest("Task2.depth", test1 Task2.depth, TestCase.tdepth);
    runTest("Task2.max", test1 Task2.max, TestCase.tmax);
    ()
end

let () = Tester.runAll()

