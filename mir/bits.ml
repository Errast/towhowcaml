external int32_to_float : int -> (float[@unboxed])
  = "mir_int32_to_double_byte" "mir_int32_to_double"
[@@noalloc]

external sign_extend : int -> int = "mir_sign_extend" [@@noalloc]

external sign_extend_8 : int -> int = "mir_sign_extend_8" [@@noalloc]
external sign_extend_16 : int -> int = "mir_sign_extend_16" [@@noalloc]

external signed_divide_32 : int -> int -> int = "mir_signed_divide_32"
[@@noalloc]

external unsigned_divide_32 : int -> int -> int = "mir_unsigned_divide_32"
[@@noalloc]

external signed_remainder_32 : int -> int -> int = "mir_signed_remainder_32"
[@@noalloc]

external unsigned_remainder_32 : int -> int -> int = "mir_unsigned_remainder_32"
[@@noalloc]

external signed_shift_right_32 : int -> int -> int = "mir_signed_shift_right_32"
[@@noalloc]

external unsigned_shift_right_32 : int -> int -> int
  = "mir_unsigned_shift_right_32"
[@@noalloc]
