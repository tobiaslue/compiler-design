open Assert
open X86
open Asm
open Simulator
open Gradedtests

(** Creates memory layout out of given instructions. *)
let sbytes_of_ins (instructions:ins list) : sbyte list =
  List.concat @@ List.map (fun i -> [InsB0 i;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]) instructions

(** Creates machine out of given instructions. *)
let machine_of_ins (instructions:ins list) : mach =
  sbytes_of_ins instructions |> test_machine

(** Creates machine out of given instructions and data. *)
let machine_of_ins2 (instructions:ins list) (data:sbyte list) : mach =
  sbytes_of_ins instructions @ data |> test_machine

let option_get = function
  | Some x -> x
  | None -> failwith "invalid address"

(** Check if memory has given value at given address. *)
let check_memory (memory:mem) (address:int64) (value:int64) : bool =
  value = int64_of_sbytes @@ Array.to_list @@ Array.sub memory (option_get @@ map_addr address) 8

let b0 = Byte '\x00'
let bff = Byte '\xff'

let inss_to_sbytes (inss:ins list) : sbyte list =
  List.flatten (List.map Simulator.sbytes_of_ins inss)

let machine_test_inss (inss:ins list) =
  inss_to_sbytes inss |> test_machine |> machine_test "" (List.length inss)

type cc_expected = CC_set | CC_cleared | CC_unchanged

let machine_test_cc (inss:ins list) (fo', fs', fz') () : unit =
  List.iter (fun init ->
    let expect : cc_expected -> bool = function
      | CC_set -> true
      | CC_cleared -> false
      | CC_unchanged -> init in
    let m = inss_to_sbytes inss |> test_machine in
    machine_test "" (List.length inss)
      {m with flags = {fo = init; fs = init; fz = init}}
      (fun {flags} ->
        expect fo' = flags.fo &&
        expect fs' = flags.fs &&
        expect fz' = flags.fz) ()
  ) [false; true]

  let cc_from_to (n:int) (m:mach) (fo',fs',fz') (fo'',fs'',fz'') = 
    cc_test (Printf.sprintf "expected OF:%b SF:%b ZF:%b" fo'' fs'' fz'')
      n m (fo',fs',fz')
      (fun m -> m.flags.fo = fo'' && m.flags.fs = fs'' && m.flags.fz = fz'')
  
  
  (* Additional shift cc tests*)
  let cc_sarq_0 = test_machine  
    [InsB0 (Movq, [~$0x400600; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Sarq, [(Imm (Lit 0L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_sarq_1 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0xFFFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Sarq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_sarq_2 = test_machine  
    [InsB0 (Movq, [~$424242; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Sarq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_sarq_3 = test_machine  
    [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Sarq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_sarq_4 = test_machine  
    [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Sarq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  
  let cc_shlq_1 = test_machine  
    [InsB0 (Movq, [~$424242; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Shlq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_shlq_2 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x3FFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Shlq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_shlq_3 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x7FFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Shlq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_shlq_4 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x7FFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Shlq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  
  let cc_shrq_1 = test_machine  
    [InsB0 (Movq, [~$424242; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Shrq, [~$0; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_shrq_2 = test_machine  
    [InsB0 (Movq, [~$(-1); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Shrq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_shrq_3 = test_machine  
    [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Shrq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_shrq_4 = test_machine  
    [InsB0 (Movq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Shrq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  (* additional overflow tests *)
  
  let cso_mult_1 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x4000000000000000L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Imulq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cso_add_1 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x7FFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Addq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cso_sub_1 = test_machine  
    [InsB0 (Movq, [(Imm (Lit 0x8000000000000000L)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Subq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let cc_additional_tests =
    [ ("cc_sarq_0", cc_from_to 2 cc_sarq_0 (false, false, false) (false, false, false));
      ("cc_sarq_1", cc_from_to 2 cc_sarq_1 (false, false, false) (false, false, false));
      ("cc_sarq_2", cc_from_to 2 cc_sarq_2 (true, false, false) (false, false, false));
      ("cc_sarq_4", cc_from_to 2 cc_sarq_4 (true, false, false) (true, false, true));
      ("cc_shlq_1", cc_from_to 2 cc_shlq_1 (true, false, true) (true, false, true));
      ("cc_shlq_2", cc_from_to 2 cc_shlq_2 (false, false, false) (false, false, false));
      ("cc_shlq_3", cc_from_to 2 cc_shlq_3 (false, false, false) (true, true, false));
      ("cc_shlq_4", cc_from_to 2 cc_shlq_4 (false, false, false) (false, true, false));
      ("cc_shrq_1", cc_from_to 2 cc_shrq_1 (false, false, false) (false, false, false));
      ("cc_shrq_2", cc_from_to 2 cc_shrq_2 (false, false, false) (true, false, false));
      ("cc_shrq_3", cc_from_to 2 cc_shrq_3 (false, false, false) (false, false, true));
      ("cc_shrq_4", cc_from_to 2 cc_shrq_4 (false, false, false) (false, false, true));
      ("cc_mult_1", cso_test 2 cso_mult_1 true); 
      ("cc_add_1", cso_test 2 cso_add_1 true); 
      ("cc_sub_1", cso_test 2 cso_sub_1 true); 
    ]
  
  (* additional functional tests *)
  
  let setb_1 = test_machine
    [InsB0 (Movq, [(Imm (Lit 0xFFFFFFFFFFFFFFFFL)); ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Cmpq, [~$2; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Set Le, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let setb_2 = test_machine
    [InsB0 (Movq, [(Imm (Lit 0xFFFFFFFFFFFFFFFFL)); Ind2 Rsp]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Addq, [~$7; ~%Rsp]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Cmpq, [~$2; ~$1]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
     ;InsB0 (Set Gt, [Ind2 Rsp]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag]
  
  let additional_func_tests = [
    ("setb_1", machine_test "only to change the lowest byte" 3 setb_1 (fun m -> 
      m.regs.(rind Rax) = 0xFFFFFFFFFFFFFF01L
    ));  
    ("setb_2", machine_test "only to change the lowest byte" 4 setb_2 (fun m -> 
      m.mem.(0xffff) = Byte(Char.chr 0)
    ));  
  ]

let provided_tests : suite = [

  Test ("Student-Provided Tests For step", [
    ("movq1", machine_test "rax=5" 1 (machine_of_ins [Movq, [~$5; ~%Rax]])
      (fun {regs} -> regs.(rind Rax) = 5L));
    ("movq2", machine_test "rax=*0x400010=65" 2 (machine_of_ins [
        Movq, [~$65; Ind1 (Lit 0x400010L)];
        Movq, [Ind1 (Lit 0x400010L); ~%Rax];
      ]) (fun {regs;mem} -> regs.(rind Rax) = 65L && mem.(16) = Byte 'A'));
    ("movq3", machine_test "rax=*0x400018=66" 3 (machine_of_ins [
        Movq, [~$0x400018; ~%Rax];
        Movq, [~$66; Ind2 Rax];
        Movq, [Ind2 Rax; ~%Rax];
      ]) (fun {regs;mem} -> regs.(rind Rax) = 66L && mem.(24) = Byte 'B'));
    ("movq4", machine_test "rax=*0x400020=67" 3 (machine_of_ins [
        Movq, [~$0x400018; ~%Rax];
        Movq, [~$67; Ind3 (Lit 8L, Rax)];
        Movq, [Ind3 (Lit 8L, Rax); ~%Rax];
      ]) (fun {regs;mem} -> regs.(rind Rax) = 67L && mem.(32) = Byte 'C'));
    ("movq5", machine_test "rax=*0x400021=MIN_INT" 3 (machine_of_ins [
        Movq, [~$0x400018; ~%Rax];
        Movq, [~$0x80; Ind3 (Lit 16L, Rax)];
        Movq, [Ind3 (Lit 9L, Rax); ~%Rax];
      ]) (fun {regs;mem} -> regs.(rind Rax) = Int64.min_int && mem.(40) = Byte '\x80'));
    (* Segfault on invalid memory access *)
    ("movq6", fun () -> try (machine_test "segfault" 1 (machine_of_ins [
        Movq, [Ind1 (Lit 0x410000L); ~%Rax];
      ]) (fun _ -> true) ());
      failwith "bad address" with X86lite_segfault -> ());
    ("movq7", fun () -> try (machine_test "segfault" 2 (machine_of_ins [
        Movq, [~$0x3ffffff; ~%Rax];
        Movq, [Ind2 Rax; ~%Rax];
      ]) (fun _ -> true) ());
      failwith "bad address" with X86lite_segfault -> ());
    ("movq8", fun () -> try (machine_test "segfault" 2 (machine_of_ins [
        Movq, [~$0x410000; ~%Rax];
        Movq, [Ind3 (Lit (-7L), Rax); ~%Rax];
      ]) (fun _ -> true) ());
      failwith "bad address" with X86lite_segfault -> ());

    ("negq1", machine_test "rax=-5" 2 (machine_of_ins [
        Movq, [~$5; ~%Rax];
        Negq, [~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = -5L));
    ("negq2", machine_test "rax=MIN_INT" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.min_int); ~%Rax];
        Negq, [~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.min_int &&
        flags.fo && flags.fs && not flags.fz));
    ("negq3", machine_test "rax=MIN_INT+1" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.max_int); ~%Rax];
        Negq, [~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.add Int64.min_int 1L &&
        not flags.fo && flags.fs && not flags.fz));

    ("incq1", machine_test "rax=5" 2 (machine_of_ins [
        Movq, [~$4; ~%Rax];
        Incq, [~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 5L));
    ("incq2", machine_test "rax=MIN_INT" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.max_int); ~%Rax];
        Incq, [~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.min_int &&
        flags.fo && flags.fs && not flags.fz));

    ("decq1", machine_test "rax=5" 2 (machine_of_ins [
        Movq, [~$6; ~%Rax];
        Decq, [~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 5L));
    ("decq2", machine_test "rax=MAX_INT" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.min_int); ~%Rax];
        Decq, [~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.max_int &&
        flags.fo && not flags.fs && not flags.fz));

    ("addq1", machine_test "rax=55" 2 (machine_of_ins [
        Movq, [~$4; ~%Rax];
        Addq, [~$51; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 55L));
    ("addq2", machine_test "rax=MIN_INT+4" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.max_int); ~%Rax];
        Addq, [~$5; ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.add Int64.min_int 4L &&
        flags.fo && flags.fs && not flags.fz));

    ("subq1", machine_test "rax=55" 2 (machine_of_ins [
        Movq, [~$100; ~%Rax];
        Subq, [~$45; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 55L));
    ("subq2", machine_test "rax=MIN_INT" 2 (machine_of_ins [
        Movq, [Imm (Lit (Int64.add Int64.min_int 3L)); ~%Rax];
        Subq, [~$3; ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.min_int &&
        not flags.fo && flags.fs && not flags.fz));
    ("subq3", machine_test "rax=MAX_INT" 2 (machine_of_ins [
        Movq, [Imm (Lit (Int64.add Int64.min_int 3L)); ~%Rax];
        Subq, [~$4; ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.max_int &&
        flags.fo && not flags.fs && not flags.fz));
    ("subq4", machine_test "rax=0" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.min_int); ~%Rax];
        Subq, [Imm (Lit Int64.min_int); ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = 0L &&
        flags.fo && not flags.fs && flags.fz));

    ("cmpq1", machine_test "rax=100" 2 (machine_of_ins [
        Movq, [~$100; ~%Rax];
        Cmpq, [~$45; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 100L));
    ("cmpq2", machine_test "rax=MIN_INT+3" 2 (machine_of_ins [
        Movq, [Imm (Lit (Int64.add Int64.min_int 3L)); ~%Rax];
        Cmpq, [~$3; ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.add Int64.min_int 3L &&
        not flags.fo && flags.fs && not flags.fz));
    ("cmpq3", machine_test "rax=MAX_INT+3" 2 (machine_of_ins [
        Movq, [Imm (Lit (Int64.add Int64.min_int 3L)); ~%Rax];
        Cmpq, [~$4; ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.add Int64.min_int 3L &&
        flags.fo && not flags.fs && not flags.fz));
    ("cmpq4", machine_test "rax=MIN_INT" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.min_int); ~%Rax];
        Cmpq, [Imm (Lit Int64.min_int); ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = Int64.min_int &&
        flags.fo && not flags.fs && flags.fz));
    (* Compq does not need a destination *)
    ("cmpq5", machine_test "OF=1 FS=0 FZ=1" 1 (machine_of_ins [
        Cmpq, [Imm (Lit Int64.min_int); Imm (Lit Int64.min_int)];
      ]) (fun {flags} -> flags.fo && not flags.fs && flags.fz));

    ("imulq1", machine_test "rax=55" 2 (machine_of_ins [
        Movq, [~$5; ~%Rax];
        Imulq, [~$11; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 55L));
    ("imulq2", machine_test "rax=MAX_INT rbx=MIN_INT" 4 (machine_of_ins [
        Movq, [Imm (Lit Int64.max_int); ~%Rax];
        Imulq, [~$1; ~%Rax];
        Movq, [Imm (Lit Int64.min_int); ~%Rbx];
        Imulq, [~$1; ~%Rbx];
      ]) (fun {flags;regs} -> not flags.fo && (* ZF and SF undefined *)
        regs.(rind Rax) = Int64.max_int &&
        regs.(rind Rbx) = Int64.min_int));
    ("imulq3", machine_test "rax=4 and OF" 2 (machine_of_ins [
        Movq, [Imm (Lit 0x4000000000000001L); ~%Rax];
        Imulq, [~$4; ~%Rax];
      ]) (fun {flags;regs} -> flags.fo (* ZF and SF undefined *) &&
        regs.(rind Rax) = 4L));
    ("imulq4", machine_test "rax=0 and OF" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.min_int); ~%Rax];
        Imulq, [~$2; ~%Rax];
      ]) (fun {flags;regs} -> flags.fo (* ZF and SF undefined *) &&
        regs.(rind Rax) = 0L));

    ("notq1", machine_test "rax=-1" 2 (machine_of_ins [
        Movq, [~$0; ~%Rax];
        Notq, [~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = -1L));
    ("notq2", machine_test "rax=MIN_INT" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.max_int); ~%Rax];
        Notq, [~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = Int64.min_int));

    ("andq1", machine_test "rax=0b00101" 2 (machine_of_ins [
        Movq, [~$0b00111; ~%Rax];
        Andq, [~$0b01101; ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = 0b00101L &&
        not flags.fo && not flags.fs && not flags.fz));
    ("andq2", machine_test "rax=0" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.min_int); ~%Rax];
        Andq, [Imm (Lit Int64.max_int); ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = 0L &&
        not flags.fo && not flags.fs && flags.fz));

    ("orq1", machine_test "rax=0b01111" 2 (machine_of_ins [
        Movq, [~$0b00111; ~%Rax];
        Orq,  [~$0b01101; ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = 0b01111L &&
        not flags.fo && not flags.fs && not flags.fz));
    ("orq2", machine_test "rax=-1" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.min_int); ~%Rax];
        Orq,  [Imm (Lit Int64.max_int); ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = -1L &&
        not flags.fo && flags.fs && not flags.fz));

    ("xorq1", machine_test "rax=0b01010" 2 (machine_of_ins [
        Movq, [~$0b00111; ~%Rax];
        Xorq, [~$0b01101; ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = 0b01010L &&
        not flags.fo && not flags.fs && not flags.fz));
    ("xorq2", machine_test "rax=-1" 2 (machine_of_ins [
        Movq, [Imm (Lit Int64.min_int); ~%Rax];
        Xorq, [Imm (Lit Int64.max_int); ~%Rax];
      ]) (fun {flags;regs} -> regs.(rind Rax) = -1L &&
        not flags.fo && flags.fs && not flags.fz));

    ("leaq1", machine_test "rax=0x400000" 1 (machine_of_ins [
        Leaq, [Ind1 (Lit 0x400000L); ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 0x400000L));
    ("leaq2", machine_test "rax=0x400010" 2 (machine_of_ins [
        Movq, [~$0x400010; ~%Rax];
        Leaq, [Ind2 Rax; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 0x400010L));
    ("leaq3", machine_test "rax=0x400020" 2 (machine_of_ins [
        Movq, [~$0x400010; ~%Rax];
        Leaq, [Ind3 (Lit 0x10L, Rax); ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 0x400020L));
    (* Invalid addresses do not segfault until accessed *)
    ("leaq4", machine_test "rax=0" 2 (machine_of_ins [
        Movq, [~$0; ~%Rax];
        Leaq, [Ind2 Rax; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 0L));
    ("leaq5", machine_test "rax=0x410000" 1 (machine_of_ins [
        Leaq, [Ind1 (Lit 0x410000L); ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 0x410000L));
    ("leaq6", fun () -> try (machine_test "segfault" 2 (machine_of_ins [
        Leaq, [Ind1 (Lit 0x410000L); ~%Rax];
        Movq, [Ind2 Rax; ~%Rax];
      ]) (fun _ -> true) ());
      failwith "bad address" with X86lite_segfault -> ());
    (* As answered by Yichen Yan on moodle *)
    ("leaq7", machine_test "rax=0x400008L" 1 (machine_of_ins [
        Leaq, [Ind2 Rip; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 0x400008L));

    ("setb1", machine_test "rax=rdx=r09=1 rbx=rcx=r08=0" 8 (machine_of_ins [
        Movq, [~$0; ~%Rax];
        Cmpq, [~$0; ~$0];
        Set Eq, [~%Rax];
        Set Neq, [~%Rbx];
        Set Gt, [~%Rcx];
        Set Ge, [~%Rdx];
        Set Lt, [~%R08];
        Set Le, [~%R09];
      ]) (fun {regs} ->
        regs.(rind Rax) = 1L && regs.(rind Rdx) = 1L && regs.(rind R09) = 1L &&
        regs.(rind Rbx) = 0L && regs.(rind Rcx) = 0L && regs.(rind R08) = 0L));
    ("setb2", machine_test "rbx=r08=r09=1 rax=rcx=rdx=0" 8 (machine_of_ins [
        Movq, [~$0; ~%Rax];
        Cmpq, [~$5; ~$(-5)];
        Set Eq, [~%Rax];
        Set Neq, [~%Rbx];
        Set Gt, [~%Rcx];
        Set Ge, [~%Rdx];
        Set Lt, [~%R08];
        Set Le, [~%R09];
      ]) (fun {regs} ->
        regs.(rind Rbx) = 1L && regs.(rind R08) = 1L && regs.(rind R09) = 1L &&
        regs.(rind Rax) = 0L && regs.(rind Rcx) = 0L && regs.(rind Rdx) = 0L));
    (* setq does only access lowest byte *)
    ("setb3", machine_test "*0x400018=0x01ffffffffffffff" 3 (machine_of_ins [
        Movq, [~$(-1); Ind1 (Lit 0x400018L)];
        Cmpq, [~$0; ~$0];
        Set Eq, [Ind1 (Lit 0x400018L)];
      ]) (fun {mem} -> Array.sub mem 24 8 = [|Byte '\x01';bff;bff;bff;bff;bff;bff;bff|]));
    ("setb4", machine_test "*0x400018=0x00ffffffffffffff" 3 (machine_of_ins [
        Movq, [~$(-1); Ind1 (Lit 0x400018L)];
        Cmpq, [~$0; ~$0];
        Set Neq, [Ind1 (Lit 0x400018L)];
      ]) (fun {mem} -> Array.sub mem 24 8 = [|b0;bff;bff;bff;bff;bff;bff;bff|]));

    ("jmp1", machine_test "rax=5" 3 (machine_of_ins [
        Movq, [~$5; ~%Rax];
        Jmp, [~$0x400000];
        Movq, [~$1337; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 5L));
    ("jmp2", machine_test "rax=5" 3 (machine_of_ins [
        Movq, [~$5; ~%Rax];
        Jmp, [~$0x400008];
        Movq, [~$1337; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 5L));
    ("jmp3", machine_test "rax=5" 4 (machine_of_ins [
        Movq, [~$5; ~%Rax];
        Movq, [~$0x400000; ~%Rbx];
        Jmp, [Reg Rbx];
        Movq, [~$1337; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 5L));

    ("j1", machine_test "rax=5" 4 (machine_of_ins [
        Movq, [~$5; ~%Rax];
        Cmpq, [~$0; ~$0];
        J Eq, [~$0x400000];
        Movq, [~$1337; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 5L));
    ("j2", machine_test "rax=1337" 4 (machine_of_ins [
        Movq, [~$5; ~%Rax];
        Cmpq, [~$0; ~$0];
        J Neq, [~$0x400000];
        Movq, [~$1337; ~%Rax];
      ]) (fun {regs} -> regs.(rind Rax) = 1337L));

    ("pushq1", machine_test "rsp=0x40fff0 *0x40fff0=5" 1 (machine_of_ins [
        Pushq, [~$5];
      ]) (fun {regs;mem} ->
        regs.(rind Rsp) = Int64.sub mem_top 16L &&
        check_memory mem (Int64.sub mem_top 16L) 5L));
    ("pushq2", machine_test "rsp=0x40ffe0 *0x40fff0=5 *0x40ffe8=13 *0x40ffe0=59" 3 (machine_of_ins [
        Pushq, [~$5];
        Pushq, [~$13];
        Pushq, [~$59];
      ]) (fun {regs;mem} ->
        regs.(rind Rsp) = Int64.sub mem_top 32L &&
        check_memory mem (Int64.sub mem_top 16L) 5L &&
        check_memory mem (Int64.sub mem_top 24L) 13L &&
        check_memory mem (Int64.sub mem_top 32L) 59L));

    ("popq1", machine_test "rax=13 rsp=0x40ffe8 *0x40fff0=5" 3 (machine_of_ins [
        Pushq, [~$5];
        Pushq, [~$13];
        Popq, [~%Rax];
      ]) (fun {regs;mem} ->
        regs.(rind Rax) = 13L &&
        regs.(rind Rsp) = Int64.sub mem_top 16L &&
        check_memory mem (Int64.sub mem_top 16L) 5L));
    ("popq2", machine_test "rax=13 rsp=0x40ffe8 *0x40fff0=5" 5 (machine_of_ins [
        Pushq, [~$5];
        Pushq, [~$13];
        Pushq, [~$59];
        Popq, [~%Rax];
        Popq, [~%Rax];
      ]) (fun {regs;mem} ->
        regs.(rind Rax) = 13L &&
        regs.(rind Rsp) = Int64.sub mem_top 16L &&
        check_memory mem (Int64.sub mem_top 16L) 5L));
    ("popq3", machine_test "rax=-1337 rsp=0x40fff8" 6 (machine_of_ins [
        Pushq, [~$27];
        Pushq, [~$42];
        Popq, [~%Rax];
        Popq, [~%Rax];
        Pushq, [~$(-1337)];
        Popq, [~%Rax];
      ]) (fun {regs;mem} ->
        regs.(rind Rax) = -1337L &&
        regs.(rind Rsp) = Int64.sub mem_top 8L));

    ("callq1", machine_test "rsp=0x40fff0 rip=0x400020 *0x40fff0=0x400008" 1 (machine_of_ins [
        Callq, [~$0x400020];
      ]) (fun {regs;mem} ->
        regs.(rind Rsp) = Int64.sub mem_top 16L &&
        regs.(rind Rip) = 0x400020L &&
        check_memory mem (Int64.sub mem_top 16L) 0x400008L));
    ("callq2", machine_test "rsp=0x40ffe8 rip=0x400030 *0x40fff0=0x400008 *0x40ffe8=0x400010" 2 (machine_of_ins [
        Callq, [~$0x400008];
        Callq, [~$0x400030];
      ]) (fun {regs;mem} ->
        regs.(rind Rsp) = Int64.sub mem_top 24L &&
        regs.(rind Rip) = 0x400030L &&
        check_memory mem (Int64.sub mem_top 16L) 0x400008L &&
        check_memory mem (Int64.sub mem_top 24L) 0x400010L));

    ("retq1", machine_test "rsp=0x40fff8 rip=0x400008 rax=5" 3 (machine_of_ins [
        Callq, [~$0x400010];
        Movq, [~$42; ~%Rax];
        Movq, [~$5; ~%Rax];
        Retq, [];
      ]) (fun {regs;mem} ->
        regs.(rind Rsp) = Int64.sub mem_top 8L &&
        regs.(rind Rip) = 0x400008L &&
        regs.(rind Rax) = 5L));
    ("retq2", machine_test "rsp=0x410000 rip=exit_addr" 5 (
      let machine = machine_of_ins [
        Callq, [~$0x400010];
        Retq, [];
        Callq, [~$0x400018];
        Retq, [];
      ] in
        (* Set top of the stack to exit_addr (equal to how it is done in load) *)
        mem_store machine.mem (Int64.sub mem_top 8L) exit_addr; machine)
      (fun {regs;mem} ->
        regs.(rind Rsp) = mem_top &&
        regs.(rind Rip) = exit_addr));
  ]);

  Test ("Instruction Tests", [
    (* unary ops *)
  ("negq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Negq, [~%Rax]]
    (fun m -> m.regs.(rind Rax) = -42L)
  );
  ("incq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Incq, [~%Rax]]
    (fun m -> m.regs.(rind Rax) = 43L)
  );
  ("decq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Decq, [~%Rax]]
    (fun m -> m.regs.(rind Rax) = 41L)
  );
  ("notq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Notq, [~%Rax]]
    (fun m -> m.regs.(rind Rax) = -43L)
  );
    (* binary ops *)
  ("addq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Addq, [~%Rbx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = 55L)
  );
  ("subq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Subq, [~%Rbx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = 29L)
  );
  ("imulq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Movq, [~$(-13); ~%Rbx]; Imulq, [~%Rbx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = -546L)
  );
  ("xorq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Xorq, [~%Rbx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = 39L)
  );
  ("orq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Orq, [~%Rbx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = 47L)
  );
  ("andq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Movq, [~$13; ~%Rbx]; Andq, [~%Rbx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = 8L)
  );
  ("sarq", machine_test_inss
    [Movq, [~$(-42); ~%Rax]; Movq, [~$2; ~%Rcx]; Sarq, [~%Rcx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = -11L)
  );
  ("shlq", machine_test_inss
    [Movq, [~$(-42); ~%Rax]; Movq, [~$2; ~%Rcx]; Shlq, [~%Rcx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = -168L)
  );
  ("shrq", machine_test_inss
    [Movq, [~$(-42); ~%Rax]; Movq, [~$2; ~%Rcx]; Shrq, [~%Rcx; ~%Rax]]
    (fun m -> m.regs.(rind Rax) = 0x3ffffffffffffff5L)
  );
    (* other ops *)
  ("leaq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Leaq, [Ind3 (Lit 13L, Rax); ~%Rbx]]
    (fun m -> m.regs.(rind Rbx) = 55L)
  );
  ("pushq", machine_test_inss
    [Movq, [~$42; ~%Rax]; Pushq, [~%Rax]]
    (fun m -> m.regs.(rind Rax) = 42L
      && m.regs.(rind Rsp) = Int64.sub mem_top 16L
      && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) = 42L)
  );
  ("popq", machine_test_inss
    [Movq, [~$42; Ind2 Rsp]; Popq, [~%Rax]]
    (fun m -> m.regs.(rind Rax) = 42L
      && m.regs.(rind Rsp) = mem_top
      && int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 42L)
  );
  ("set true", machine_test_inss
    [Movq, [Imm (Lit 0x123456789abcdefL); ~%Rax]; Cmpq, [~$1; ~$2]; Set Gt, [~%Rax]]
    (fun m -> m.regs.(rind Rax) = 0x123456789abcd01L)
  );
  ("set false", machine_test_inss
    [Movq, [Imm (Lit 0x123456789abcdefL); ~%Rax]; Cmpq, [~$2; ~$2]; Set Gt, [~%Rax]]
    (fun m -> m.regs.(rind Rax) = 0x123456789abcd00L)
  );
  ("set mem", machine_test_inss
    [Movq, [Imm (Lit 0x123456789abcdefL); Ind2 Rsp]; Cmpq, [~$1; ~$2]; Set Gt, [Ind2 Rsp]]
    (fun m -> int64_of_sbytes (sbyte_list m.mem (mem_size-8)) = 0x123456789abcd01L)
  );
  ("set at mem top does not cause segfault", machine_test_inss
    [Addq, [~$7; ~%Rsp]; Cmpq, [~$1; ~$2]; Set Gt, [Ind2 Rsp]]
    (fun m -> (m.mem.(mem_size-1)) = Byte (Char.chr 1))
  );
  ("jmp", machine_test_inss
    [Jmp, [~$42]]
    (fun m -> m.regs.(rind Rip) = 42L)
  );
  ("j true", machine_test_inss
    [Cmpq, [~$1; ~$2]; J Gt, [~$42]]
    (fun m -> m.regs.(rind Rip) = 42L)
  );
  ("j false", machine_test_inss
    [Cmpq, [~$2; ~$2]; J Gt, [~$42]]
    (fun m -> m.regs.(rind Rip) = Int64.add mem_bot 16L)
  );
  ("retq", machine_test_inss
    [Movq, [~$42; Ind2 Rsp]; Retq, []]
    (fun m -> m.regs.(rind Rip) = 42L
      && m.regs.(rind Rsp) = mem_top)
  );
  ("callq", machine_test_inss
    [Callq, [~$42]]
    (fun m -> m.regs.(rind Rip) = 42L
      && m.regs.(rind Rsp) = Int64.sub mem_top 16L
      && int64_of_sbytes (sbyte_list m.mem (mem_size-16)) =
        Int64.add mem_bot 8L)
  );
  (* missing: Movq, Cmpq *)
  ]);
    (* fo, fs, fz *)
  Test ("Condition Flag Set Tests", [
  ("notq should not touch flags", machine_test_cc
    [Movq, [~$42; ~%Rax]; Notq, [~%Rax]]
    (CC_unchanged, CC_unchanged, CC_unchanged)
  );
  ("imulq no overflow", machine_test_inss
    [Movq, [~$42; ~%Rax]; Movq, [~$(-13); ~%Rbx]; Imulq, [~%Rbx; ~%Rax]]
    (fun m -> m.flags.fo = false)
  );
  ("imulq overflow", machine_test_inss
    [Movq, [Imm (Lit 0x0001000000000000L); ~%Rax];  Imulq, [~%Rax; ~%Rax]]
    (fun m -> m.flags.fo = true)
  );
   (* if AMT=0 flags are unaffected *)
  ("sarq-flags-amt0", machine_test_cc
    [Movq, [~$(-42); ~%Rax]; Sarq, [~$0; ~%Rax]]
    (CC_unchanged, CC_unchanged, CC_unchanged)
  );
  ("shlq-flags-amt0", machine_test_cc
    [Movq, [~$(-42); ~%Rax]; Shlq, [~$0; ~%Rax]]
    (CC_unchanged, CC_unchanged, CC_unchanged)
  );
  ("shrq-flags-amt0", machine_test_cc
    [Movq, [~$(-42); ~%Rax]; Shrq, [~$0; ~%Rax]]
    (CC_unchanged, CC_unchanged, CC_unchanged)
  );
   (* if AMT=1 then fo=0, fs and fz normal*)
  ("sarq-flags-amt1", machine_test_cc
    [Movq, [~$(-42); ~%Rax]; Movq, [~$1; ~%Rcx]; Sarq, [~%Rcx; ~%Rax]]
    (CC_cleared, CC_set, CC_cleared)
  );
   (* OF is set if the top two bits of DEST are different and the shift amount is 1 *)
  ("shlq-flags-amt1-01", machine_test_cc
    [Movq, [Imm (Lit 0x4000000000000000L); ~%Rax]; Shlq, [~$1; ~%Rax]]
    (CC_set, CC_set, CC_cleared)
  );
  ("shlq-flags-amt1-10", machine_test_cc
    [Movq, [Imm (Lit 0x8000000000000000L); ~%Rax]; Shlq, [~$1; ~%Rax]]
    (CC_set, CC_cleared, CC_set)
  );
  ("shlq-flags-amt1-00", machine_test_cc
    [Movq, [~$0; ~%Rax]; Shlq, [~$1; ~%Rax]]
    (CC_unchanged, CC_cleared, CC_set)
  );
  ("shlq-flags-amt1-11", machine_test_cc
    [Movq, [Imm (Lit 0xc000000000000000L); ~%Rax]; Shlq, [~$1; ~%Rax]]
    (CC_unchanged, CC_set, CC_cleared)
  );
   (* OF is set to the most-significant bit of the original operand if the shift amount is 1 *)
  ("shrq-flags-amt1", machine_test_cc
    [Movq, [~$1; ~%Rax]; Shrq, [~$1; ~%Rax]]
    (CC_cleared, CC_cleared, CC_set)
  );
  ("shrq-flags-amt1", machine_test_cc
    [Movq, [~$(-1); ~%Rax]; Shrq, [~$1; ~%Rax]]
    (CC_set, CC_cleared, CC_cleared)
  );
   (* if AMT<>1 then fo is unaffected *)
  ("sarq-flags-amt3", machine_test_cc
    [Movq, [~$(-42); ~%Rax]; Sarq, [~$3; ~%Rax]]
    (CC_unchanged, CC_set, CC_cleared)
  );
  ("shlq-flags-amt3", machine_test_cc
    [Movq, [~$(-42); ~%Rax]; Shlq, [~$3; ~%Rax]]
    (CC_unchanged, CC_set, CC_cleared)
  );
  ("shrq-flags-amt3", machine_test_cc
    [Movq, [~$(-42); ~%Rax]; Shrq, [~$3; ~%Rax]]
    (CC_unchanged, CC_cleared, CC_cleared)
  );
  ]);

  Test ("Additonal cc tests", cc_additional_tests);
  Test ("Additonal functionality tests", additional_func_tests);

]
