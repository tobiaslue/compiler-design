open Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let tests = [
  ("subarraysum.oat", "", "90")
]

let provided_tests : suite = [
  GradedTest ("subarraysum", 5, executed_oat_file tests);
] 
