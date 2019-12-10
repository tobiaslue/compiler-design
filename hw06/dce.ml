(** Dead Code Elimination  *)
open Ll
open Datastructures


(* expose a top-level analysis operation ------------------------------------ *)
(* TASK: This function should optimize a block by removing dead instructions
   - lb: a function from uids to the live-OUT set at the
     corresponding program point
   - ab: the alias map flowing IN to each program point in the block
   - b: the current ll block

   Note:
     Call instructions are never considered to be dead (they might produce
     side effects)

     Store instructions are not dead if the pointer written to is live _or_
     if some alias of that pointer is live.

     Other instructions are dead if the value they compute is not live.

   Hint: Consider using List.filter
 *)
let dce_block (lb:uid -> Liveness.Fact.t)
              (ab:uid -> Alias.fact)
              (b:Ll.block) : Ll.block =
  let f = fun (id, ins) ->
    begin match ins with
      |Call _ -> true
      |Store (_, _, op) ->
        begin match op with
          |Id x |Gid x ->
            begin match (UidM.mem x (ab id), UidS.mem x (lb id)) with
              |true, true ->
                let aliased = UidM.find x (ab id) in
                begin match aliased with
                  |Alias.SymPtr.Unique -> true
                  |Alias.SymPtr.MayAlias -> true
                  |_ -> false
                end
              |_ -> false
            end
          |_ -> invalid_arg "operand must be uid"
        end
      |_ -> UidS.mem id (lb id)
    end in
  let new_insns = List.filter f b.insns in
  {
    insns = new_insns;
    term = b.term
  }

let run (lg:Liveness.Graph.t) (ag:Alias.Graph.t) (cfg:Cfg.t) : Cfg.t =

  LblS.fold (fun l cfg ->
    let b = Cfg.block cfg l in

    (* compute liveness at each program point for the block *)
    let lb = Liveness.Graph.uid_out lg l in

    (* compute aliases at each program point for the block *)
    let ab = Alias.Graph.uid_in ag l in

    (* compute optimized block *)
    let b' = dce_block lb ab b in
    Cfg.add_block l b' cfg
  ) (Cfg.nodes cfg) cfg
