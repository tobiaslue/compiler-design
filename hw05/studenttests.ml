open Assert
open Ast
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.    *)


let unit_tests = [
  "subtype_int_int",
  (fun () ->
     if Typechecker.subtype Tctxt.empty (TInt) (TInt) then ()
     else failwith "should not fail")
]



let unit_tests = [
  ("CArr expression",
    (fun () -> 
      try
        if (Typechecker.typecheck_exp Tctxt.empty (no_loc (CArr (TInt, [(no_loc (CInt 8L))]))) = TRef (RArray TInt)) then ()
        else failwith "typecheck_exp returned wrong type"
      with Typechecker.TypeError _ -> failwith "should not fail"
    )
  );
  ("no CArr expression",
    (fun () ->
      try
        if (Typechecker.typecheck_exp Tctxt.empty (no_loc (CArr (TInt, [(no_loc (CBool true))]))) = TRef (RArray TInt)) then failwith "should fail"
        else ();
        failwith "should fail"
      with Typechecker.TypeError _ -> ();
    ) 
  )
]

let provided_tests : suite = [
  GradedTest("Student provided unit tests", 5, unit_tests); 
  GradedTest("Student provided oat testcase", 5, executed_oat_file [ ("priorityqueue.oat", "", "370")]);
] 
