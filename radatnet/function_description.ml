open Ctypes

module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct 
  open F

  let r_core_new = foreign "r_core_new" (void @-> returning Types.r2_core)
  let r_core_cmd_str = foreign "r_core_cmd_str" (string @-> Types.r2_core @-> returning string)
  let r_core_free = foreign "r_core_free" (Types.r2_core @-> returning void)
end