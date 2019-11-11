open Ll
open Llutil
open Ast

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for
     compiling local variable declarations
*)

type elt =
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
    let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
        match e with
        | L l ->
           begin match term_opt with
           | None ->
              if (List.length insns) = 0 then (gs, einsns, [], None, blks)
              else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                               no terminator" l
           | Some term ->
              (gs, einsns, [], None, (l, {insns; term})::blks)
           end
        | T t  -> (gs, einsns, [], Some (Llutil.Parsing.gensym "tmn", t), blks)
        | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
        | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
        | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
    in
    match term_opt with
    | None -> failwith "build_cfg: entry block has no terminator"
    | Some term ->
       let insns = einsns @ insns in
       ({insns; term}, blks), gs


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    List.assoc id c

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    match List.assoc id c with
    | Ptr (Fun (args, ret)), g -> Ptr (Fun (args, ret)), g
    | _ -> failwith @@ id ^ " not bound to a function"

  let lookup_function_option (id:Ast.id) (c:t) : (Ll.ty * Ll.operand) option =
    try Some (lookup_function id c) with _ -> None

end

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the the corresponding integer types. OAT strings are
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the LLVM type checker.
*)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]
  | Ast.RFun (ts, t) ->
      let args, ret = cmp_fty (ts, t) in
      Fun (args, ret)

and cmp_ret_ty : Ast.ret_ty -> Ll.ty = function
  | Ast.RetVoid  -> Void
  | Ast.RetVal t -> cmp_ty t

and cmp_fty (ts, r) : Ll.fty =
  List.map cmp_ty ts, cmp_ret_ty r


let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)


(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Amount of space an Oat type takes when stored in the satck, in bytes.
   Note that since structured values are manipulated by reference, all
   Oat values take 8 bytes on the stack.
*)
let size_oat_ty (t : Ast.ty) = 8L

(* Generate code to allocate an array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let arr_ty = Ptr I64 in
  ans_ty, Id ans_id, lift
    [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
    ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]


(* Compiles an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression.

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to bitcast the
     resulting gid to (Ptr I8)

   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions

   - we found it useful to write a helper function
     cmp_exp_as : Ctxt.t -> Ast.exp node -> Ll.ty -> Ll.operand * stream
     that compiles an expression and optionally inserts a bitcast to the
     desired Ll type. This is useful for dealing with OAT identifiers that
     correspond to gids that don't quite have the type you want

*)
let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
  begin match exp.elt with
    |CNull ty -> cmp_ty ty, Null, []
    |CBool b -> I1, Const (if b then 1L else 0L), []
    |CInt x -> I64, Const x, []
    |CStr s -> failwith "CStr unimplemented"

    |CArr _ -> failwith "CArr unimplemented"
    |NewArr _ -> failwith "NewArr unimplemented"
    |Id x ->
      let (ty, op) = Ctxt.lookup x c in
      let uid = gensym "" in
      begin match ty with
        |Ptr(ty) -> ty, Id(uid), [I(uid, Load(Ptr(ty), op))]
        |_ -> failwith "array not implemented"
      end
    |Index _ -> failwith "Index unimplemented"
    |Bop (op, x1, x2) ->
      let (ty1, op1, s1) = cmp_exp c x1 in
      let (ty2, op2, s2) = cmp_exp c x2 in
      let var = (gensym "") in
      begin match op with
        |Add -> I64, Id var, [I (var, (Binop (Add, I64, op1, op2)))] @ s2 @ s1
        |Sub -> I64, Id var, [I (var, (Binop (Sub, I64, op1, op2)))] @ s2 @ s1
        |Mul -> I64, Id var, [I (var, (Binop (Mul, I64, op1, op2)))] @ s2 @ s1
        |Eq -> I1, Id var, [I (var, (Icmp (Eq, I64, op1, op2)))] @ s2 @ s1
        |Neq -> I1, Id var, [I (var, (Icmp (Ne, I64, op1, op2)))] @ s2 @ s1
        |Lt -> I1, Id var, [I (var, (Icmp (Slt, I64, op1, op2)))] @ s2 @ s1
        |Lte -> I1, Id var, [I (var, (Icmp (Sle, I64, op1, op2)))] @ s2 @ s1
        |Gt -> I1, Id var, [I (var, (Icmp (Sgt, I64, op1, op2)))] @ s2 @ s1
        |Gte -> I1, Id var, [I (var, (Icmp (Sge, I64, op1, op2)))] @ s2 @ s1
        |And -> I1, Id var, [I (var, (Binop (And, I1, op1, op2)))] @ s2 @ s1
        |Or -> I1, Id var, [I (var, (Binop (Or, I1, op1, op2)))] @ s2 @ s1
        |IAnd -> I64, Id var, [I (var, (Binop (And, I64, op1, op2)))] @ s2 @ s1
        |IOr -> I64, Id var, [I (var, (Binop (Or, I64, op1, op2)))] @ s2 @ s1
        |Shl -> I64, Id var, [I (var, (Binop (Shl, I64, op1, op2)))] @ s2 @ s1
        |Shr -> I64, Id var, [I (var, (Binop (Lshr, I64, op1, op2)))] @ s2 @ s1
        |Sar -> I64, Id var, [I (var, (Binop (Ashr, I64, op1, op2)))] @ s2 @ s1
      end
    |Uop (op, x) ->
      let (ty, op1, s) = cmp_exp c x in
      let var = (gensym "") in
      begin match op with
        |Neg -> I64, Id var, [I (var, (Binop (Mul, I64, op1, Const (Int64.neg 1L))))] @ s
        |Lognot -> I1, Id var, [I (var, (Binop (Xor, I1, op1, Const (Int64.one))))] @ s
        |Bitnot -> I64, Id var, [I (var, (Binop (Xor, I64, op1, Const (Int64.minus_one))))] @ s
      end
    |Call (f, args) ->
      let g = fun (argsll, stream1) arg ->
        let (tyarg, oparg, streamarg) = cmp_exp c arg in
        argsll @ [(tyarg, oparg)], stream1 @ streamarg in
      let f = begin match f.elt with
        |Id x -> x
        |_ -> invalid_arg "wrong operand for call"
      end in
      let (ty, op) = Ctxt.lookup_function f c in
      begin match ty with
        |Ptr Fun (arg_types, ret_type) ->
          let (args, stream) = List.fold_left g ([], []) args in
          let var = gensym "" in
          ret_type, Id var, [I (var, (Call (ret_type, op, args)))] @ stream
        |_ -> invalid_arg "wrong type for call"
      end
  end


(* Compile a statement in context c with return typ rt. Return a new context,
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add a bindings to the context for local variable
     declarations

   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement

   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it!

 *)
let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) (stmt:Ast.stmt node) : Ctxt.t * stream =
  let make_stmts return_type (c, stream) stmt : (Ctxt.t * stream) =
    let (c, s) = cmp_stmt c return_type stmt in
    c, s @ stream in
  begin match stmt.elt with
    |Ret x ->
      begin match x with
        |Some e ->
          let (ty, op, stream) = cmp_exp c e in
          (c, T(Ret (ty, Some op)) :: stream)
        |None -> c, [T(Ret (Void, None))]
      end
    |Decl (id, e) ->
      let (ty, op, stream) = cmp_exp c e in
      let uid = gensym "" in
      Ctxt.add c id (Ptr ty, Id uid),
      [I ("", (Store (ty, op, (Id uid))))] @ [E (uid, (Alloca ty))] @ stream
    |If (e, s1, s2) ->
      let (ty, op, stream) = cmp_exp c e in
      let if_lbl = gensym "if" in
      let else_lbl = gensym "else" in
      let end_lbl = gensym "end" in
      let (c, stream1) = List.fold_left (make_stmts rt) (c, []) s1 in
      let (c, stream2) = List.fold_left (make_stmts rt) (c, []) s2 in
      begin match s2 with
        |[] ->
          c, stream2 @ [L else_lbl] @
             [T (Br else_lbl)] @ stream1 @ [L if_lbl] @
             [T (Cbr (op, if_lbl, else_lbl))] @ stream
        |_ ->
          c, [L end_lbl] @
             [T (Br end_lbl)] @ stream2 @ [L else_lbl] @
             [T (Br end_lbl)] @ stream1 @ [L if_lbl] @
             [T (Cbr (op, if_lbl, else_lbl))] @ stream
      end
    |While (e, s) ->
      let (ty, op, stream) = cmp_exp c e in
      let cnd_lbl = gensym "cnd" in
      let end_lbl = gensym "end" in
      let loop_lbl = gensym "loop" in
      let (c, stream1) = List.fold_left (make_stmts rt) (c, []) s in
      c, [L end_lbl] @
         [T (Br cnd_lbl)] @ stream1 @ [L loop_lbl] @
         [T (Cbr (op, loop_lbl, end_lbl))] @ stream @ [L cnd_lbl] @
         [T (Br cnd_lbl)]
    |Assn (le, re) ->
      let (tyr, opr, streamr) = cmp_exp c re in (*evaluate le and store op in le instead of var. Fix expression id first*)
      begin match le.elt with
        |Id id ->
          let (tyl, opl) = Ctxt.lookup id c in
          c, [I ("", Store (tyr, opr, opl))] @ streamr
        |_ -> failwith"array not implemented"
      end
    |For (vdecls, e, inc, s) ->
      (*Declaration*)
      let (id, edecl) = match (List.hd vdecls) with (id, e) -> id, e in
      let (ty, op, stream) = cmp_exp c edecl in
      let uid = gensym "" in
      let c = Ctxt.add c id (Ptr ty, Id uid) in
      let decl = [I ("", (Store (ty, op, (Id uid))))] @
                 [E (uid, (Alloca ty))] @ stream in

      (*Operations for increment*)
      let inc = match inc with
        |Some x -> x
        |_ -> failwith"invalid increment statement in for loop" in
      let (le, re) = match inc.elt with
        |Assn (le, re) -> le, re
        |_ -> failwith"Invalid increment statement in for loop" in
      let (tyr, opr, streamr) = cmp_exp c re in
      let (tyl, opl) = Ctxt.lookup id c in

      (*While loop*)
      let e = match e with
        |Some x -> x
        |_ -> failwith "Invalid expression in for loop" in
      let (ty, op, stream) = cmp_exp c e in
      let cnd_lbl = gensym "cnd" in
      let end_lbl = gensym "end" in
      let loop_lbl = gensym "loop" in
      let (c, stream1) = List.fold_left (make_stmts rt) (c, []) s in
      let loop = [L end_lbl] @ [T (Br cnd_lbl)] @
                 [I ("", Store (tyr, opr, opl))] @ streamr @ (*increment*)
                 stream1 @ [L loop_lbl] @
                 [T (Cbr (op, loop_lbl, end_lbl))] @ stream @ [L cnd_lbl] @
                 [T (Br cnd_lbl)] in

      c, loop @ decl
    |SCall (f, args) ->
      let e = no_loc (Call (f, args)) in
      let (ty, op, stream) = cmp_exp c e in
      c, stream
  end


(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : stream =
  snd @@ List.fold_left (fun (c, code) s ->
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts



(* Adds each function identifer to the context at an
   appropriately translated type.

   NOTE: The Gid of a function is just its source name
*)
let cmp_function_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
    List.fold_left (fun c -> function
      | Ast.Gfdecl { elt={ frtyp; fname; args } } ->
         let ft = TRef (RFun (List.map fst args, frtyp)) in
         Ctxt.add c fname (cmp_ty ft, Gid fname)
      | _ -> c
    ) c p


(* Populate a context with bindings for global variables
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs. (The constructors starting with C).
*)

let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  let f acc x = match x with
    | Gvdecl g ->
      let name = g.elt.name in
      let init = g.elt.init.elt in
      let bin =
        begin match init with
          | CNull ty -> (Ptr (cmp_ty ty), Gid name)
          | CBool b -> (Ptr I1, Gid name)
          | CInt i -> (Ptr I64, Gid name)
          | CStr s -> (Ptr (Array(1 + (String.length s), I8)), Gid name)
          | CArr (ty, e) -> failwith "cmp_global_ctxt: CArr not implemented"
          | _ -> failwith "cmp_global_ctxt: invalid type of global initializer"
        end
      in
      Ctxt.add acc name bin
    | _ -> acc
  in
  List.fold_left f c p

(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   3. Compile the body of the function using cmp_block
   4. Use cfg_of_stream to produce a LLVMlite cfg from
*)

let cmp_fdecl (c:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let f = f.elt in
  let f_ty = cmp_fty ((List.map fst f.args), f.frtyp) in
  let f_param = List.map snd f.args in
  let g = fun (c, acc) (ty, id) ->
    let uid = gensym id in
    let ty = cmp_ty ty in
    let c = Ctxt.add c id (Ptr ty, Id uid) in
    c, acc @ [E("", Store (ty, (Id id), (Id uid)))] @ [E(uid, Alloca ty)]
  in
  let (c, prologue) = List.fold_left g (c, []) f.args in
  let body = cmp_block c (cmp_ret_ty f.frtyp) f.body in
  let f_cfg, gs = cfg_of_stream @@ prologue @ body in
  {f_ty; f_param; f_cfg}, gs



(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations
*)
let rec cmp_gexp c (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  match e.elt with
  | CNull ty -> (cmp_ty ty, GNull), []
  | CBool b -> (I1, GInt (if b then 1L else 0L)), []
  | CInt i -> (I64, GInt i), []
  | CStr s -> (Array(1 + (String.length s), I8), GString s), []
  | CArr _ -> failwith "unimplemented"
  | _ -> failwith "cmp_gexp: invalid type of global initializer"


(* Oat internals function context ------------------------------------------- *)
let internals = [
    "oat_alloc_array",         Ll.Fun ([I64], Ptr I64)
  ]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  [ "array_of_string",  cmp_rty @@ RFun ([TRef RString], RetVal (TRef(RArray TInt)))
  ; "string_of_array",  cmp_rty @@ RFun ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", cmp_rty @@ RFun ([TRef RString],  RetVal TInt)
  ; "string_of_int",    cmp_rty @@ RFun ([TInt],  RetVal (TRef RString))
  ; "string_cat",       cmp_rty @@ RFun ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     cmp_rty @@ RFun ([TRef RString],  RetVoid)
  ; "print_int",        cmp_rty @@ RFun ([TInt],  RetVoid)
  ; "print_bool",       cmp_rty @@ RFun ([TBool], RetVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt =
    List.fold_left (fun c (i, t) -> Ctxt.add c i (Ll.Ptr t, Gid i))
      Ctxt.empty builtins
  in
  let fc = cmp_function_ctxt init_ctxt p in

  (* build global variable context *)
  let c = cmp_global_ctxt fc p in

  (* compile functions and global variables *)
  let fdecls, gdecls =
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } ->
           let ll_gd, gs' = cmp_gexp c gd.init in
           (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
           let fdecl, gs' = cmp_fdecl c fd in
           (fd.elt.fname,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in

  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls = []; gdecls; fdecls; edecls }
