open Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let tests = [
  ("knapsack.oat", "", "480")
]

let provided_tests : suite = [
  GradedTest ("knapsack", 5, executed_oat_file tests);
] 
