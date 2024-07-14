#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim double mir_int32_to_double(value arg) {
  int value = Int_val(arg);
  return *((float*)&value);
}
CAMLprim value mir_int32_to_double_byte(value arg) {
  return caml_copy_double(mir_int32_to_double(arg));
}

CAMLprim value mir_sign_extend_8(value arg) {
  long value = (unsigned int) (signed char) Long_val(arg);
  return Val_long(value);
}

CAMLprim value mir_sign_extend_16(value arg) {
  long value = (unsigned int) (signed short) Long_val(arg);
  return Val_long(value);
}

CAMLprim value mir_sign_extend(value arg) {
  long value = (int) Long_val(arg);
  return Val_long(value);
}

CAMLprim value mir_signed_divide_32(value arg1, value arg2) {
  long value = (int)Long_val(arg1) / (int)Long_val(arg2);
  return Val_long(value);
}

CAMLprim value mir_unsigned_divide_32(value arg1, value arg2) {
  long value = ((unsigned int)Long_val(arg1)) / (unsigned int)Long_val(arg2);
  return Val_long(value);
}

CAMLprim value mir_signed_remainder_32(value arg1, value arg2) {
  long value = (int)Long_val(arg1) % (int)Long_val(arg2);
  return Val_long(value);
}

CAMLprim value mir_unsigned_remainder_32(value arg1, value arg2) {
  long value = ((unsigned int)Long_val(arg1)) % (unsigned int)Long_val(arg2);
  return Val_long(value);
}

CAMLprim value mir_signed_shift_right_32(value arg1, value arg2) {
  long value = (unsigned int)((int)Long_val(arg1) >> (int)Long_val(arg2));
  return Val_long(value);
}

CAMLprim value mir_unsigned_shift_right_32(value arg1, value arg2) {
  long value = (unsigned int)Long_val(arg1) >> (unsigned int)Long_val(arg2);
  return Val_long(value);
}
