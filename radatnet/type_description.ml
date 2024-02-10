open Ctypes

module Types (F: Ctypes.TYPE) = struct 
  open F

  type r2_core = unit ptr
  let r2_core = ptr void
end