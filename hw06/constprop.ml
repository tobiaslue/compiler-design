open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
  struct
    type t = NonConst           (* Uid may take on multiple values at runtime *)
           | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
           | UndefConst         (* Uid is not defined at the point *)

    let compare s t =
      match (s, t) with
      | (Const i, Const j) -> Int64.compare i j
      | (NonConst, NonConst) | (UndefConst, UndefConst) -> 0
      | (NonConst, _) | (_, UndefConst) -> 1
      | (UndefConst, _) | (_, NonConst) -> -1

    let to_string : t -> string = function
      | NonConst -> "NonConst"
      | Const i -> Printf.sprintf "Const (%LdL)" i
      | UndefConst -> "UndefConst"


  end

(* The analysis computes, at each program point, which UIDs in scope will evaluate
   to integer constants *)
type fact = SymConst.t UidM.t



(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
 *)
let insn_flow (u,i:uid * insn) (d:fact) : fact =
  let f = fun op d ->
    begin match op with
      |Null -> SymConst.UndefConst
      |Const x -> SymConst.Const x
      |Id id |Gid id ->
        begin match UidM.mem id d with
          |true -> UidM.find id d
          |false -> SymConst.UndefConst
        end
    end in
  begin match i with
    |Binop (operation, _, op1, op2) ->
      let c1 = f op1 d in
      let c2 = f op2 d in
      let c = begin match (c1, c2) with
        |SymConst.NonConst, _
        |_, SymConst.NonConst -> SymConst.NonConst
        |SymConst.UndefConst, _
        |_, SymConst.UndefConst -> SymConst.UndefConst
        |SymConst.Const x, SymConst.Const y ->
          begin match operation with
            |Add -> SymConst.Const (Int64.add x y)
            |Sub -> SymConst.Const (Int64.sub x y)
            |Mul -> SymConst.Const (Int64.mul x y)
            |Shl -> SymConst.Const (Int64.shift_left x (Int64.to_int y))
            |Lshr -> SymConst.Const (Int64.shift_right_logical x (Int64.to_int y))
            |Ashr -> SymConst.Const (Int64.shift_right x (Int64.to_int y))
            |And -> SymConst.Const (Int64.logand x y)
            |Or -> SymConst.Const (Int64.logor x y)
            |Xor -> SymConst.Const (Int64.logxor x y)
          end
      end in
      UidM.add u c d
    |Icmp (cnd, _, op1, op2) ->
    let c1 = f op1 d in
    let c2 = f op2 d in
    let c = begin match (c1, c2) with
      |SymConst.NonConst, _
      |_, SymConst.NonConst -> SymConst.NonConst
      |SymConst.UndefConst, _
      |_, SymConst.UndefConst -> SymConst.UndefConst
      |SymConst.Const x, SymConst.Const y ->
        begin match cnd with
          |Eq -> if (Int64.compare x y) = 0 then SymConst.Const 1L else SymConst.Const 0L
          |Ne -> if (Int64.compare x y) <> 0 then SymConst.Const 1L else SymConst.Const 0L
          |Slt -> if (Int64.compare x y) < 0 then SymConst.Const 1L else SymConst.Const 0L
          |Sle -> if (Int64.compare x y) <= 0 then SymConst.Const 1L else SymConst.Const 0L
          |Sgt -> if (Int64.compare x y) > 0 then SymConst.Const 1L else SymConst.Const 0L
          |sge -> if (Int64.compare x y) >= 0 then SymConst.Const 1L else SymConst.Const 0L
        end
      end in
      UidM.add u c d
    |Call (void, _, _) -> UidM.add u SymConst.UndefConst d
    |Store _ -> UidM.add u SymConst.UndefConst d
    |_ -> UidM.add u SymConst.NonConst d
  end

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow

    let normalize : fact -> fact =
      UidM.filter (fun _ v -> v != SymConst.UndefConst)

    let compare (d:fact) (e:fact) : int  =
      UidM.compare SymConst.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymConst.to_string v)

    (* The constprop analysis should take the join over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let combine (ds:fact list) : fact =
      let join = fun id f1 f2 ->
        begin match (f1, f2) with
          |None, None -> None
          |Some x, None
          |None, Some x -> Some x
          |Some x, Some y ->
            begin match (x, y) with
              |SymConst.NonConst, NonConst
              |SymConst.Const _, SymConst.NonConst
              |SymConst.NonConst, SymConst.Const _ -> Some SymConst.NonConst
              |SymConst.Const a, SymConst.Const b ->
                if (a = b) then Some (SymConst.Const a)
                else Some SymConst.NonConst
              |SymConst.UndefConst, _
              |_, SymConst.UndefConst -> Some SymConst.UndefConst
            end
        end in
      List.fold_left (fun map f -> UidM.merge join map f) UidM.empty ds
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right
    (fun (u,_) -> UidM.add u SymConst.NonConst)
    g.Cfg.args UidM.empty
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg


(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper
   functions.                                                                 *)
let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in


  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    failwith "Constprop.cp_block unimplemented"
  in

  LblS.fold cp_block (Cfg.nodes cfg) cfg
