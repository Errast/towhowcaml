#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <r_core.h>
#include <x86.h>
#include <stdint.h>
#include <stdio.h>

void radatnet_core_free(value core) {
	r_core_free(*((void**) Data_custom_val(core)));
}

static const struct custom_operations radare_core_ocaml_ops = {
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
	char* result = r_core_cmd_str(*((RCore**) Data_custom_val(core)), String_val(string));
	actual_result = caml_copy_string(result);
	free(result);
	CAMLreturn(actual_result);
}

value radatnet_core_cmd_str_at(value core, value address, value string) {
  CAMLparam3(core,address, string);
  CAMLlocal1(actual_result);
  char* result = r_core_cmd_str_at(*((RCore**) Data_custom_val(core)), Long_val(address), String_val(string));
  actual_result = caml_copy_string(result);
  free(result);
  CAMLreturn(actual_result);
}

enum x86_insn_extensions {
	X86_INS_FADDP = 1524,
  X86_INS_CMPLTPD
};

value radatnet_core_anal_op(value core, value address) {
  CAMLparam2(core, address);
  CAMLlocal2(opex, op_value);
  RCore *core_ptr =  *((RCore**) Data_custom_val(core));
  uint8_t buffer[15];
  r_io_read_at(core_ptr->io, Long_val(address), buffer, sizeof(buffer));
  RAnalOp op;
  r_anal_op(core_ptr->anal, &op, Long_val(address), buffer, sizeof(buffer), R_ARCH_OP_MASK_OPEX /*| R_ARCH_OP_MASK_VAL*/);

  switch(op.id) {
  	case X86_INS_FADD:
  		if(buffer[0] == 0xDE) {
  			op.id = X86_INS_FADDP;
  		}
  		break;
  	case X86_INS_CMPPS:
  		if(buffer[0] == 0x66) {
  			switch(buffer[op.size - 1]) {
  				case 1:
  					op.id = X86_INS_CMPLTPD;
  					break;
  			}
  		}
  		break;
  }
   
  opex = caml_alloc_initialized_string(op.opex.len, R_STRBUF_SAFEGET(&op.opex));
  op_value = caml_alloc_small(5, 0);
  Store_field(op_value, 0, Val_long(op.addr));
  Store_field(op_value, 1, Val_int(op.prefix));
  Store_field(op_value, 2, Val_int(op.id));
  Store_field(op_value, 3, opex);
  Store_field(op_value, 4, Val_int(op.size));
  r_anal_op_fini(&op);
  CAMLreturn(op_value);
}
