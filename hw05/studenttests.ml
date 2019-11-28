open Assert

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.    *)


let unit_tests = [
  "subtype_int_int",
  (fun () ->
     if Typechecker.subtype Tctxt.empty (TInt) (TInt) then ()
     else failwith "should not fail")
]



let provided_tests : suite = [
  GradedTest("student subtype unit tests", 0, unit_tests);
]
