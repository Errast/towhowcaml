open C

type t = { mutable disposed : bool; core : Types.r2_core }

exception Radatnet_disposed

let create () =
  let core = Functions.r_core_new () in
  Ctypes.raw_address_of_ptr core |> Nativeint.to_string |> print_endline;
  { disposed = false; core }

let destroy t =
  if not t.disposed then (
    t.disposed <- true;
    Functions.r_core_free t.core)

let run t str =
  if t.disposed then raise Radatnet_disposed
  else (print_endline str; Functions.r_core_cmd_str str t.core)
