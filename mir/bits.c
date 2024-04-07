#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim double towhowcaml_int32_to_double(value arg) {
  int value = Int_val(arg);
  return *((float*)&value);
}
CAMLprim value towhowcaml_int32_to_double_byte(value arg) {
  return caml_copy_double(towhowcaml_int32_to_double(arg));
}
