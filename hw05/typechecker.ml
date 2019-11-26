open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  match t1, t2 with
    | TInt, TInt -> true
    | TBool, TBool -> true
    | TNullRef r1, TNullRef r2 -> subtype_ref c r1 r2
    | TRef r1, TRef r2 -> subtype_ref c r1 r2
    | TRef r1, TNullRef r2 -> subtype_ref c r1 r2
    | _ -> false

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
  match t1, t2 with
    | RString, RString -> true
    | RArray t1, RArray t2 -> t1 = t2
    | RStruct s1, RStruct s2 ->
      begin match (lookup_struct_option s1 c), (lookup_struct_option s2 c) with
        | Some _, Some _ -> true
        | _ -> false
      end
    | RFun (ts1, rt1), RFun (ts2, rt2) ->
      let args = List.fold_left (fun acc (t1, t2) ->
        acc && subtype c t1 t2
      ) true (List.combine ts1 ts2)
      in
      args && subtype_rt c rt1 rt2
    | _ -> false

and subtype_rt (c : Tctxt.t) (t1 : Ast.ret_ty) (t2 : Ast.ret_ty) : bool =
  match t1, t2 with
    | RetVoid, RetVoid -> true
    | RetVal rt1, RetVal rt2 -> subtype c rt1 rt2
    | _ -> false


(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the 
      type is 

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
    | TInt -> ()
    | TBool -> ()
    | TRef r -> typecheck_ref l tc r
    | TNullRef r -> typecheck_ref l tc r

and typecheck_ref (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  match t with
    | RString -> ()
    | RArray t -> typecheck_ty l tc t
    | RStruct id ->
      begin match lookup_struct_option id tc with
        | Some _ -> ()
        | None -> type_error l "struct not in contex"
      end 
    | RFun (tys, rty) -> 
      List.iter (typecheck_ty l tc) tys;
      typecheck_rt l tc rty

and typecheck_rt (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ret_ty) : unit =
  match t with 
    | RetVoid -> ()
    | RetVal t -> typecheck_ty l tc t

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oad.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  let exp = e.elt in
  match exp with
    | CNull r -> 
      typecheck_ref e c r;
      TNullRef r
    | CBool _ -> TBool
    | CInt _ -> TInt
    | CStr _ -> TRef RString

    | Id id -> 
      begin match lookup_option id c with
        | Some ty -> ty 
        | None -> type_error e "variable not in scope"
      end

    | CArr (ty, es) -> 
      let ts = List.map (typecheck_exp c) es in
      List.iter (fun t -> if not (subtype c t ty) then type_error e "wrong element type") ts;
      typecheck_ty e c ty;
      TRef (RArray ty)

    | NewArr (ty, e1, id, e2) -> 
      begin match lookup_local_option id c with 
      | Some _ -> type_error e "variable already defined"
      | None ->
        let t1 = typecheck_exp c e1 in
        let tc = add_local c id TInt in
        let t2 = typecheck_exp tc e2 in
        if (subtype c t2 ty && t1 = TInt) then TRef (RArray ty)
        else type_error e "wrong array type" 
      end

    | Index (e1, e2) -> 
      let t1 = typecheck_exp c e1 in
      let t2 = typecheck_exp c e2 in
      let t = begin match t1, t2 with
        | TRef (RArray ty), TInt -> ty
        | _ -> type_error e "index wrong type"
        end
      in
      t

    | Length e1 ->
      let t1 = typecheck_exp c e1 in
      begin match t1 with
        | TRef (RArray ty) -> TInt
        | _ -> type_error e "length wrong type"
      end

    | CStruct _ -> failwith "typecheck_exp: CStruct not implemented"

    | Proj (e, id) ->
      begin match typecheck_exp c e with
        | TRef (RStruct s) -> 
          begin match lookup_field_option s id c with
            | Some ty -> ty 
            | None -> type_error e "Struct or field not in context"
          end
        | _ -> type_error e "wrong struct type"
      end

    | Call (e, es) ->
      let ts = List.map (typecheck_exp c) es in
      begin match typecheck_exp c e with
        | TRef (RFun (tys, RetVal ty)) -> 
          List.iter2 (fun t1 t2 -> if not (subtype c t1 t2) then type_error e "wrong function type") tys ts;
          ty
        | _ -> type_error e "wrong function type"
      end

    | Bop (Eq, e1, e2) | Bop (Neq, e1, e2) ->
      let t1 = typecheck_exp c e1 in
      let t2 = typecheck_exp c e2 in
      if (subtype c t1 t2 && subtype c t2 t1) then TBool
      else type_error e "expressions not matching"

    | Bop (bop, e1, e2)-> 
      let t1, t2, tres = typ_of_binop bop in
      if (t1, t2) = ((typecheck_exp c e1), (typecheck_exp c e2)) then tres
      else type_error e "expressions not matching"

    | Uop (uop, e1) ->
      let t1, tres = typ_of_unop uop in
      if t1 = typecheck_exp c e1 then tres
      else type_error e "expressions not matching"

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statment typechecking rules from oat.pdf.  

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement
     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, both branches must definitely return

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entier conditional statement might 
        not return.
  
        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  match s.elt with
    | Assn (lhs, e) ->
      let _ = begin match lhs.elt with
        | Id id -> 
          begin match lookup_global_option id tc with
            | Some _ -> type_error s ""
            | None -> ()
          end
        | _ -> ()
      end
      in
      let t1 = typecheck_exp tc lhs in
      let t2 = typecheck_exp tc e in
      if subtype tc t1 t2 then tc, false
      else type_error s "wrong assignment"

    | Decl (id, e) -> 
      let _ = match lookup_local_option id tc with
        | Some _ -> type_error s "variable already defined"
        | None -> ()
      in
      let t = typecheck_exp tc e in
      (add_local tc id t), false

    | SCall (e, es) -> 
      let ts1 = List.map (typecheck_exp tc) es in
      begin match typecheck_exp tc e with
        | TNullRef RFun (ts2, RetVoid) -> 
          List.iter2 (fun t1 t2 ->
            if not (subtype tc t1 t2) then type_error s "wrong arguments"
          ) ts1 ts2;
          tc, false
        | _ -> type_error s "wrong function type"
      end

    | If (e, b1, b2) -> 
      let t = typecheck_exp tc e in
      let _, r1 = typecheck_block tc b1 to_ret in
      let _, r2 = typecheck_block tc b2 to_ret in
      begin match t with
        | TBool -> tc, r1 && r2
        | _ -> type_error s "expression must be of type bool"
      end 

    | While (e, b)->
      let t = typecheck_exp tc e in
      let _ = typecheck_block tc b to_ret in
      begin match t with
        | TBool -> tc, false
        | _ -> type_error s "expression must be of type bool"
      end

    | For (vs, e_opt, s_opt, b) ->
      let c_new = List.fold_left (fun c (id, exp) ->
        begin match lookup_local_option id c with
          | Some _ -> type_error s "variable already defined"
          | None ->
            let t = typecheck_exp c exp in
            add_local c id t
        end
      ) tc vs in
      begin match e_opt with
        | Some e -> if (TBool <> typecheck_exp c_new e) then type_error e "must be of type bool"
        | None -> ()
      end;
      begin match s_opt with
        | Some s -> 
          let _ = typecheck_stmt c_new s to_ret in ()
        | None -> ()
      end;
      let _ = typecheck_block c_new b to_ret in
      tc, false

    | Ret r ->
      begin match r, to_ret with
        | None, RetVoid -> tc, true
        | Some e, RetVal ty ->
          let t = typecheck_exp tc e in
          if subtype tc t ty then tc, true
          else type_error s "wrong return type"
        | _ -> type_error s "wrong return type"
      end

    | Cast _ -> failwith "unimplemented"
    
and typecheck_block (tc : Tctxt.t) (b:Ast.block) (to_ret:ret_ty) : Tctxt.t * bool =
  match b with
    | [] -> tc, false
    | [s] -> typecheck_stmt tc s to_ret
    | s1::s2::ss -> 
      let c, _ = typecheck_stmt tc s1 to_ret in
      typecheck_block c (s2::ss) to_ret

(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) id fs  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)


let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
  let frty = f.frtyp in
  let args = f.args in
  let body = f.body in
  let c = List.fold_left (fun c arg ->
    add_local c (snd arg) (fst arg)
  ) tc args in
  let _, r = typecheck_block c body frty in
  if not r then type_error l "No return statement"

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'S'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'F' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can't mention other global values *)

let create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun c p ->
    match p with
      | Gtdecl t ->
        let id = fst t.elt in
        let fields = snd t.elt in
        begin match lookup_struct_option id c with
          | Some _ -> failwith "Struct already defined"
          | None -> add_struct c id fields
        end
      | _ -> c

  ) empty p

let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun c p -> 
    match p with
      | Gfdecl f ->
        let id = f.elt.fname in
        let tys = List.map fst f.elt.args in
        let rty = f.elt.frtyp in
        let ty = TRef (RFun (tys, rty)) in
        begin match lookup_global_option id c with
          | Some _ -> failwith "Function already defined"
          | None -> add_global c id ty
        end
      | _ -> c
  ) tc p 

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun c p ->
    match p with 
      | Gvdecl g -> 
        let id = g.elt.name in
        let ty = typecheck_exp tc g.elt.init in
        begin match lookup_global_option id c with
          | Some _ -> failwith "Global identifier already defined"
          | None -> add_global c id ty
        end
      | _ -> c
  ) tc p


(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
    | _ -> ()) p
