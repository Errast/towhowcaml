include module type of Types.Instr

val value_type : t -> Types.local_type
val replace_var : t -> Types.variable -> t
val replace_instr_ref : t -> from:Ref.t -> into:Ref.t -> t
val is_pure : t -> bool
