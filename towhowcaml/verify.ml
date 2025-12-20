open! Core
open Smt_util

type solver = Z3.Solver.solver

let reset_contexts :
    expr Option_array.t -> int -> Mir.Symbolic_eval.ctx * Patchouli.ctx =
 fun vals eip ->
  let p_ctx = Patchouli.empty_context () in
  let p_regs =
    p_ctx.reg_map
    |> Patchouli.Reg_map.set ~key:`eip ~data:Bitvec.(of_int (dword ()) eip)
  in
  let p_ctx = { p_ctx with reg_map = p_regs } in
  let m_globals =
    Patchouli.Reg_map.fold
      ~f:(fun ~key ~data acc ->
        match key with
        | `eip -> acc
        | #Radatnet.X86reg.reg_32bit ->
            Map.add_exn acc
              ~key:(Patchouli.Phys_reg.to_ident key)
              ~data:(Lazy.force data)
        | _ -> acc)
      ~init:(Map.empty (module String))
      p_regs
  in
  let fpu_stack_ptr = fresh Util.fpu_stack_pointer (Bitvec.dword ()) in
  let m_globals =
    Map.add_exn m_globals ~key:Util.fpu_stack_pointer ~data:fpu_stack_ptr
  in
  let m_plane1 =
    Patchouli.Reg_map.fold
      ~f:
        Bitvec.(
          fun ~key ~data acc ->
            match key with
            | #Patchouli.Phys_reg.floats as key ->
                let index =
                  Core.(Patchouli.Phys_reg.float_to_fp_index key * 8)
                in
                acc.%[fpu_stack_ptr + of_int (dword ()) index] <-
                  Lazy.force data
            | _ -> acc)
      ~init:(Array.uninterpreted "m_plane1" (Bitvec.dword ()) (Bitvec.byte ()))
      p_regs
  in
  let m_mems =
    Map.singleton (module Int) 0 p_ctx.memory
    |> Map.add_exn ~key:1 ~data:m_plane1
  in
  ({ vals; memory = m_mems; vars = m_globals }, p_ctx)

let assert_contexts :
    solver ->
    Mir.Symbolic_eval.ctx ->
    m_fp_stack_start:expr ->
    m_eip:expr ->
    Patchouli.ctx ->
    unit =
 fun solver m_ctx ~m_fp_stack_start ~m_eip p_ctx ->
  let m_plane0 = Map.find_exn m_ctx.memory 0 in
  let m_plane1 = Map.find_exn m_ctx.memory 1 in
  let memories_equal =
    for_all (Bitvec.dword ())
      Smt.(fun ptr -> Array.get m_plane0 ptr = Array.get p_ctx.memory ptr)
  in
  let m_fp_sp = Map.find_exn m_ctx.vars Util.fpu_stack_pointer in

  let regs_equal =
    Patchouli.Reg_map.fold p_ctx.reg_map
      ~f:
        Smt.(
          fun ~key ~data acc ->
            match key with
            | `eip -> (Lazy.force data = m_eip) :: acc
            | #Radatnet.X86reg.reg_32bit as key ->
                (Lazy.force data
                = Map.find_exn m_ctx.vars @@ Patchouli.Phys_reg.to_ident key)
                :: acc
            | #Radatnet.X86reg.flags | #Patchouli.Phys_reg.floats -> acc)
      ~init:[]
    |> and_
  in
  let fp_equal =
    and_
    @@
    if p_ctx.fp_offset = p_ctx.min_fp_offset then []
    else
      List.init
        (p_ctx.fp_offset - (p_ctx.min_fp_offset + 1))
        ~f:(fun i ->
          let mir =
            load_address m_plane1 8
              Bitvec.(m_fp_sp + of_int (dword ()) Core.(i * 8))
          in
          let patchouli =
            p_ctx.fp_offset - i |> Patchouli.Phys_reg.fp_index_to_phys_reg
            |> Patchouli.Reg_map.get p_ctx.reg_map
          in
          Smt.(mir = patchouli))
  in
  let fp_sp_equal =
    Smt.(
      Bitvec.(m_fp_sp - m_fp_stack_start = of_int (dword ()) p_ctx.fp_offset))
  in
  Z3.Solver.add solver
    [ not_ @@ and_ [ memories_equal; regs_equal; fp_equal; fp_sp_equal ] ]
