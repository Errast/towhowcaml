open! Core
open Radatnet

type regs = [ X86reg.t | X86reg.flags ] [@@deriving sexp]

type expr =
  | Ident of string
  | Int of int
  | Reg of regs
  | FunCall of string * expr list
  | Mul of expr * expr
  | Add of expr * expr
  | Eq of expr * expr
[@@deriving sexp]

type statement =
  | Assign of string * expr
  | AssignReg of regs * expr
  | FunCall of string * expr list
  | If of { cond : expr; then_ : statement list; else_ : statement list }
[@@deriving sexp]

type value =
  | True
  | False
  | Undefined
  | Int of int
  | Atom of string
  | Register of X86reg.t
  | SymbolicBV of (Smt_util.expr[@sexp.opaque])
  | SymbolicBool of (Smt_util.expr[@sexp.opaque])
  | SymbolicFloat of (Smt_util.expr[@sexp.opaque])
[@@deriving sexp, equal]

module Phys_reg = struct
  type floats =
    [ (* Indexes go from -7 to 7 *)
      (* These are negatives totally *)
      `st__1
    | `st__2
    | `st__3
    | `st__4
    | `st__5
    | `st__6
    | `st__7
    | X86reg.x87_float ]
  [@@deriving equal, compare, sexp, enumerate]

  type t = [ X86reg.reg_32bit | X86reg.flags | floats ]
  [@@deriving equal, compare, sexp, enumerate]

  include Comparator.Make (struct
    type nonrec t = t

    let sexp_of_t = sexp_of_t
    let compare = compare
  end)

  let to_sort : [< t ] -> Z3.Sort.sort = function
    | #X86reg.reg_32bit -> Smt_util.Bitvec.dword ()
    | #X86reg.flags -> Smt_util.bool ()
    | #X86reg.x87_float
    | `st__1 | `st__2 | `st__3 | `st__4 | `st__5 | `st__6 | `st__7 ->
        Smt_util.FP.double ()

  let symb_to_value : t -> Smt_util.expr -> value =
   fun t symb ->
    match t with
    | #X86reg.reg_32bit -> SymbolicBV symb
    | #X86reg.flags -> SymbolicBool symb
    | #X86reg.x87_float
    | `st__1 | `st__2 | `st__3 | `st__4 | `st__5 | `st__6 | `st__7 ->
        SymbolicFloat symb

  let to_ident : [< t ] -> string = function
    | #X86reg.t as reg -> X86reg.to_ident reg
    | #X86reg.flags as reg ->
        let[@warning "-8"] (Sexp.Atom str) = X86reg.sexp_of_flags reg in
        str
    | `st__1 -> "st(-1)"
    | `st__2 -> "st(-2)"
    | `st__3 -> "st(-3)"
    | `st__4 -> "st(-4)"
    | `st__5 -> "st(-5)"
    | `st__6 -> "st(-6)"
    | `st__7 -> "st(-7)"

  let fp_index_to_phys_reg = function
    | -7 -> `st__7
    | -6 -> `st__6
    | -5 -> `st__5
    | -4 -> `st__4
    | -3 -> `st__3
    | -2 -> `st__2
    | -1 -> `st__1
    | 0 -> `st0
    | 1 -> `st1
    | 2 -> `st2
    | 3 -> `st3
    | 4 -> `st4
    | 5 -> `st5
    | 6 -> `st6
    | _ -> failwith "invalid fp_index"

  let float_to_fp_index : [< floats ] -> int = function
    | `st__7 -> -7
    | `st__6 -> -6
    | `st__5 -> -5
    | `st__4 -> -4
    | `st__3 -> -3
    | `st__2 -> -2
    | `st__1 -> -1
    | `st0 -> 0
    | `st1 -> 1
    | `st2 -> 2
    | `st3 -> 3
    | `st4 -> 4
    | `st5 -> 5
    | `st6 -> 6
end

module Reg_map : sig
  type t

  val empty : unit -> t
  val get : t -> Phys_reg.t -> Smt_util.expr
  val set : t -> key:Phys_reg.t -> data:Smt_util.expr -> t
  val set_lazy : t -> key:Phys_reg.t -> data:Smt_util.expr lazy_t -> t
  val set_undefined : t -> Phys_reg.t -> t

  val fold_symmetric_diff :
    t ->
    t ->
    init:'a ->
    f:
      ('a ->
      Phys_reg.t ->
      left:Smt_util.expr lazy_t ->
      right:Smt_util.expr lazy_t ->
      'a) ->
    'a

  val fold :
    t ->
    init:'a ->
    f:(key:Phys_reg.t -> data:Smt_util.expr lazy_t -> 'a -> 'a) ->
    'a
end = struct
  type data = Smt_util.expr lazy_t
  type t = (Phys_reg.t, data, Phys_reg.comparator_witness) Map.t

  let phys_regs = Set.of_list (module Phys_reg) Phys_reg.all

  let undefined reg =
    let name = "fresh" ^ Phys_reg.to_ident reg in
    let sort = Phys_reg.to_sort reg in
    lazy (Smt_util.fresh name sort)

  let empty : unit -> t = fun () -> Map.of_key_set phys_regs ~f:undefined

  let[@inline] get : t -> Phys_reg.t -> Smt_util.expr =
   fun t reg -> Map.find_exn t reg |> Lazy.force

  let set t ~key ~data = Map.set t ~key ~data:(lazy data)
  let set_lazy t ~key ~data = Map.set t ~key ~data

  let set_undefined : t -> Phys_reg.t -> t =
   fun t reg -> Map.set t ~key:reg ~data:(undefined reg)

  let fold_symmetric_diff l r ~init ~f =
    Map.fold_symmetric_diff l r ~data_equal:phys_equal
      ~f:(fun acc d ->
        match d with
        | _, `Left _ | _, `Right _ -> failwith "you can't remove/add phys_regs"
        | key, `Unequal (left, right) -> f acc key ~left ~right)
      ~init

  let fold = Map.fold
end
