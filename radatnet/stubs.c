#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <r_core.h>

void radatnet_core_free(value core) {
	r_core_free(*((void**) Data_custom_val(core)));
}

static struct custom_operations radare_core_ocaml_ops = {
	"radatnet.r_core",
	radatnet_core_free,
	custom_compare_default,
	custom_hash_default,
	custom_serialize_default,
	custom_compare_ext_default,
	custom_fixed_length_default
};

value radatnet_core_new(value unit) {
	CAMLparam1(unit);
	value core = caml_alloc_custom(&radare_core_ocaml_ops, sizeof(void*), 1, 16);
	*((void**) Data_custom_val(core)) = r_core_new();
	CAMLreturn(core);
}

value radatnet_core_cmd_str(value core, value string) {
	CAMLparam2(string, core);
  CAMLlocal1(actual_result);
	// String better not have null bytes in it 
	char* result = r_core_cmd_str(*((void**) Data_custom_val(core)), String_val(string));
	actual_result = caml_copy_string(result);
	free(result);
	CAMLreturn(actual_result);
}

value radatnet_core_cmd_str_at(value core, value address, value string) {
  CAMLparam3(core,address, string);
  CAMLlocal1(actual_result);
  char* result = r_core_cmd_str_at(*((void**) Data_custom_val(core)), Long_val(address), String_val(string));
  actual_result = caml_copy_string(result);
  free(result);
  CAMLreturn(actual_result);
}
