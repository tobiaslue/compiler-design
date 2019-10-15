open Assert
open X86
open Simulator
open Asm

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let gcd a b =         
        [ text "main"
                        [ Movq, [~$a; ~%Rdi]
                        ; Movq, [~$b; ~%Rsi]
                        ; Callq, [~$$"gcd"]
                        ; Retq, []
                        ]
            ; text "gcd"
                        [ Cmpq, [~%Rdi; ~$0]
                        ; J Gt, [~$$"wrongexit"]
                        ; Cmpq, [~%Rsi; ~$0]
                        ; J Gt, [~$$"wrongexit"]
                        ; Cmpq, [~%Rdi; ~$0]
                        ; J Eq, [~$$"exit"]
                        ; Cmpq, [~%Rsi; ~$0]
                        ; J Eq, [~$$"exit"]
                        ; Cmpq, [~%Rdi; ~%Rsi]
                        ; J Lt, [~$$"agreater"]
                        ; Jmp, [~$$"asmaller"]
                        ; Retq, []
                        ]    
            ; text "agreater"
                        [ Subq, [~%Rsi; ~%Rdi]
                        ; Callq, [~$$"gcd"]
                        ; Retq, []
                        ]   
            ; text "asmaller"
                        [ Subq, [~%Rdi; ~%Rsi]
                        ; Callq, [~$$"gcd"]
                        ; Retq, []
                        ]
            ; text "wrongexit"
                        [ Movq, [~$0; ~%Rax]
                        ; Retq, []
                        ]
            ; text "exit"
                        [Movq, [~%Rdi; ~%Rax]
                        ; Retq, []
                        ]
        ]


let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
    ("gcd1", program_test (gcd 10 15) 5L);
    ("gcd2", program_test (gcd 10 10) 10L);
    ("gcd3", program_test (gcd 64 48) 16L);
    ("gcd4", program_test (gcd (-1) 10) 0L);
    ("gcd5", program_test (gcd 5 (-1)) 0L);
    ("gcd6", program_test (gcd (-65) 56) 0L);
    ("gcd7", program_test (gcd 39 11) 1L);
    ("gcd8", program_test (gcd 1311 5472) 57L);
    ("gcd9", program_test (gcd 0 1) 0L);
  ]);

] 
