(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up four bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next sevent bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 7th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. *)
let debug_simulator = ref true

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = function
  | Eq -> fz
  | Neq -> not fz
  | Lt -> fs <> fo
  | Le -> (fs <> fo) || fz
  | Gt -> (fs = fo) && (not fz)
  | Ge -> (fs = fo)


(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  if (addr < mem_bot) || (addr > mem_top) then None
  else Some ((Int64.to_int addr) - (Int64.to_int mem_bot))

let incr_rip (m:mach) = 
  m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 8L

let mem_check : int option -> int = function
   | Some x -> x
   | None -> raise X86lite_segfault

let mem_load (m:mem) (addr:quad) : int64 =
  let read_quad = 
    Array.sub m (mem_check (map_addr addr)) 8 
  in
  int64_of_sbytes (Array.to_list read_quad)

let mem_store (m:mem) (addr:quad) (v:int64) : unit = 
  let dest = mem_check (map_addr addr) in
  let data = sbytes_of_int64 v in
  m.(dest) <- List.nth data 0;
  m.(dest + 1) <- List.nth data 1;
  m.(dest + 2) <- List.nth data 2;
  m.(dest + 3) <- List.nth data 3;
  m.(dest + 4) <- List.nth data 4;
  m.(dest + 5) <- List.nth data 5;
  m.(dest + 6) <- List.nth data 6;
  m.(dest + 7) <- List.nth data 7

let resolve_ind (m:mach) (op:operand) : int64 = 
  match op with
    | Ind1 (Lit x) -> x
    | Ind2 x -> m.regs.(rind x)
    | Ind3 (Lit offset, reg) -> Int64.add (m.regs.(rind reg)) offset 
    | _ -> invalid_arg "resolve_ind: not an ind"

let store_data (m:mach) (op:operand) (v:int64) : unit =
  match op with
    | Reg x -> m.regs.(rind x) <- v
    | Ind1 (Lit x) -> mem_store m.mem x v
    | Ind2 x -> mem_store m.mem (m.regs.(rind x)) v
    | Ind3 (Lit offset, reg) -> mem_store m.mem (Int64.add (m.regs.(rind reg)) offset ) v
    | _ -> invalid_arg "store_data: tried to store to invalid operand"

let set_flags (m:mach) (res:Int64_overflow.t) = 
  m.flags.fo <- res.Int64_overflow.overflow;
  m.flags.fs <- (res.Int64_overflow.value < Int64.zero);
  m.flags.fz <- (res.Int64_overflow.value = Int64.zero)
  (*Printf.printf "Addq: res -> %d flags -> fo = %b, fs= %b, fz = %b\n"
    (Int64.to_int res.Int64_overflow.value)
    m.flags.fo
    m.flags.fs
    m.flags.fz*)

let set_flags_log (m:mach) (res:int64) =
  m.flags.fo <- false;
  m.flags.fs <- res < Int64.zero;
  m.flags.fz <- res = Int64.zero

let interpret_operand (m:mach) (ops:operand list) (i:int): int64 =
  let op = List.nth ops i in
  match op with
    | Imm (Lit x) -> x 
    | Reg x -> m.regs.(rind x)
    | Ind1 (Lit x) -> mem_load m.mem x
    | Ind2 x -> mem_load m.mem (m.regs.(rind x))
    | Ind3 (Lit offset, reg) -> mem_load m.mem (Int64.add (m.regs.(rind reg)) offset )
    | _ -> invalid_arg "interpret_operand: tried to interpret a lable!"

let store_byte (m:mach) (op:operand) (v:int64) : unit =
  let open Int64 in 
  let open Char in
  match op with
    | Reg x ->  let old = logand m.regs.(rind x) 0xFFFFFFFFFFFFFF00L in
                  m.regs.(rind x) <- logor old (logand 0xFFL v)
    | Ind1 (Lit x) ->           let dest = mem_check (map_addr x) in
                                  m.mem.(dest) <- Byte (v |> to_int |> chr)
    | Ind2 x ->                 let dest = mem_check (map_addr m.regs.(rind x)) in
                                  m.mem.(dest) <- Byte (v |> to_int |> chr)
    | Ind3 (Lit offset, reg) -> let dest = mem_check (map_addr (Int64.add (m.regs.(rind reg)) offset )) in
                                  m.mem.(dest) <- Byte (v |> to_int |> chr)
    | _ -> invalid_arg "store_byte: tried to store to invalid operand"
                  

let interpret_arith (m:mach) (i:opcode) (ops:operand list) : unit = 
  match i with
    | Negq -> let dest = interpret_operand m ops 0 in
              let res = Int64_overflow.neg dest in
                store_data m (List.nth ops 0) res.Int64_overflow.value;
                set_flags m res

    | Addq -> let src = interpret_operand m ops 0 in
              let dest = interpret_operand m ops 1 in
              let res = Int64_overflow.add src dest in
                store_data m (List.nth ops 1) res.Int64_overflow.value;
                set_flags m res

    | Subq -> let src = interpret_operand m ops 0 in
              let dest = interpret_operand m ops 1 in
              let res = Int64_overflow.sub dest src in
                store_data m (List.nth ops 1) res.Int64_overflow.value;
                set_flags m res

    | Imulq -> let src = interpret_operand m ops 0 in
               let dest = interpret_operand m ops 1 in
               let res = Int64_overflow.mul src dest in
                 store_data m (List.nth ops 1) res.Int64_overflow.value;
                 set_flags m res
              
    | Incq -> let src = interpret_operand m ops 0 in
              let res = Int64_overflow.succ src in
                store_data m (List.nth ops 0) res.Int64_overflow.value;
                set_flags m res
              
    | Decq -> let src = interpret_operand m ops 0 in
              let res = Int64_overflow.pred src in
                store_data m (List.nth ops 0) res.Int64_overflow.value;
                set_flags m res
    | _ -> invalid_arg "interpret_arith: not an arithmetic instruction"

let interpret_log (m:mach) (i:opcode) (ops:operand list): unit = 
  match i with
    | Notq -> let src = interpret_operand m ops 0 in
              let res = Int64.lognot src in
                store_data m (List.nth ops 0) res

    | Andq -> let src = interpret_operand m ops 0 in
              let dest = interpret_operand m ops 1 in
              let res = Int64.logand src dest in 
                store_data m (List.nth ops 1) res;
                set_flags_log m res
    
    | Orq -> let src = interpret_operand m ops 0 in
             let dest = interpret_operand m ops 1 in
             let res = Int64.logor src dest in 
               store_data m (List.nth ops 1) res;
               set_flags_log m res

    | Xorq -> let src = interpret_operand m ops 0 in
              let dest = interpret_operand m ops 1 in
              let res = Int64.logxor src dest in 
                store_data m (List.nth ops 1) res;
                set_flags_log m res
    | _ -> invalid_arg "interpret_log: not a logic instruction"

let interpret_bit (m:mach) (i:opcode) (ops:operand list) : unit = 
  let amt = Int64.to_int (interpret_operand m ops 0) in
  let dest = interpret_operand m ops 1 in 
  match i with
    | Sarq -> let res = Int64.shift_right dest amt in
                store_data m (List.nth ops 1) res;

    | Shlq -> let res = Int64.shift_left dest amt in
                store_data m (List.nth ops 1) res;

    | Shrq -> let res = Int64.shift_right_logical dest amt in
                store_data m (List.nth ops 1) res;

    | Set cc -> let dest = List.nth ops 0 in
                  store_byte m dest (if interp_cnd m.flags cc then Int64.one else Int64.zero)
                      
    | _ -> invalid_arg "interpret_bit: not a bit-manipultation instruction"

let interpret_data (m:mach) (i:opcode) (ops:operand list) : unit = 
  match i with
    | Leaq -> let ind = resolve_ind m (List.nth ops 0) in
                store_data m (List.nth ops 1) ind

    | Movq -> let src = interpret_operand m ops 0 in
                store_data m (List.nth ops 1) src

    | Pushq ->  let src = interpret_operand m ops 0 in
                  m.regs.(rind Rsp) <- Int64.sub m.regs.(rind Rsp) 8L;
                  mem_store m.mem m.regs.(rind Rsp) src
            
    | Popq -> let data = mem_load m.mem m.regs.(rind Rsp) in
                store_data m (List.nth ops 0) data;
                m.regs.(rind Rsp) <- Int64.add m.regs.(rind Rsp) 8L;

    | _ -> failwith "unimplemented"
    

let interpret_control (m:mach) (i:opcode) (ops:operand list) : unit = 
  match i with
    | Cmpq -> let src = interpret_operand m ops 0 in
              let dest = interpret_operand m ops 1 in
              let res = Int64_overflow.sub dest src in
                set_flags m res;
                incr_rip m;

    | Jmp ->  let src = interpret_operand m ops 0 in
                m.regs.(rind Rip) <- src

    | Callq ->  let src = interpret_operand m ops 0 in
                  interpret_data m Pushq [Reg Rip];
                  m.regs.(rind Rip) <- src

    | Retq -> interpret_data m Popq [Reg Rip]

    | J cc -> let src = interpret_operand m ops 0 in  
                m.regs.(rind Rip) <- if interp_cnd m.flags cc then src 
                                                              else Int64.add m.regs.(rind Rip) 8L
    | _ -> invalid_arg "interpret_control: not a control-flow and condition instruction"

let interpret_instr (m:mach) ((instr, op):ins) : unit =
  match instr with
    | Negq | Addq | Subq | Imulq | Incq | Decq -> interpret_arith m instr op; incr_rip m
    | Notq | Andq | Orq | Xorq -> interpret_log m instr op; incr_rip m
    | Sarq | Shlq | Shrq | Set _ -> interpret_bit m instr op; incr_rip m
    | Leaq | Movq | Pushq | Popq -> interpret_data m instr op; incr_rip m;
    | Cmpq | Jmp | Callq | Retq | J _ -> interpret_control m instr op

let eval_sbyte (m:mach) (s:sbyte) : unit = 
  match s with
    | InsB0 x -> interpret_instr m x;
    | InsFrag -> m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) Int64.one
    | Byte x -> m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) Int64.one

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
    *)

let step (m:mach) : unit =
  let instr_addr = m.regs.(rind Rip) in
  let index = mem_check (map_addr instr_addr) in
  eval_sbyte m m.mem.(index)



(* Runs the machine until the rip register reaches a designated
   memory address. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
failwith "assemble unimplemented"

(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
failwith "load unimplemented"
