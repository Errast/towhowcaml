let special = {|
  fn func0x00461aa2(int esp) -> (int esp) {
    
  }
|}

let elbrun_code = {|
  fn __int_memset__(int dest val count) {
    int back_count back_dest main_count main_i, vec splat ->
     if not count return

     if count < 4 {
       back_count = count
       back_dest = dest
     } else {
       back_count = count & 3
       main_count = (count & 0xFFFFFFFC) 
       back_dest = (main_count << 2) + dest
       splat = vsplat i32 val
       main_i = main_count 

       loop {
         store vec dest, splat
         dest = dest + 16
         main_i = main_i - 4
       } while main_i

       if main_count == count return
     }

    loop {
      store back_dest, val
      back_dest = back_dest + 4
      back_count = back_count - 1
    } while back_count
  }

  fn __int_diff__(int src dest count) -> (int src dest count) {
    int src_end dest_end ->
    if eqz count {
      return
    }

    %byte_count = count << 2
    src_end = src + %byte_count
    dest_end = dest + %byte_count

    loop {
      if load src != load dest {
        return
      }
      src = src + 4
      dest = dest + 4
      count = count - 1
    } while count

    src = src_end
    dest = dest_end
    count = 0
  }

  fn __byte_diff__(int src dest count) -> (int src dest count) {
    int src_end dest_end ->
    if eqz count {
      return
    }

    src_end = src + count
    dest_end = dest+count

    loop {
      if load i8 src != load i8 dest { 
        return
      }

      src = src + 1
      dest = dest + 1
      count = count - 1
    } while count
    
    src = src_end
    dest = dest_end
    count = 0
  }
|}

let load_big_float = {|
  -- based on the implementation in the zig compiler_rt
  fn __load_big_float__(int float_ptr) -> (float res) {
    long lower_bits mantissa res_mantissa rounded_part, 
    int upper_bits exponent,
    long result ->

    lower_bits = load long float_ptr
    mantissa = lower_bits &L 0x7FFFFFFFFFFFFFFFL
    upper_bits = load:8 i16 float_ptr
    exponent = upper_bits & 0x7FFF
    -- 15361 = 16383 - 1023 + 1
    %bot_bound = (exponent - 15361) & 0xFFFF
    -- 17407 = 16383 + 2048 - 1023
    %top_bound = (exponent - 17407) & 0xFFFF
    
    -- checks if exponent in range for f64
    -- equivalent to exponent - f80_exp_bias <= f64_exp_bias && exponent - f80_exp_bias >= -f64_exp_bias + 1 
    if %bot_bound < %top_bound {
      -- shift exp and sign into place
      -- not sure why we use upper_bits here instead of exponent, keeps the sign but we put that in anyway
      %res_exponent = extend upper_bits <<L 52L
      res_mantissa = mantissa >>L 11L
      result = %res_exponent |L res_mantissa

      -- round extra precision
      rounded_part = lower_bits &L 0x07FFL
      if rounded_part >L 0x0400L {
        result = result +L 0x4000000000000001L
      } else {
        result = result +L 0x4000000000000000L
        if rounded_part ==L 0x0400L {
          result = result +L (res_mantissa &L 1L)
        }
      }
    } else if (lower_bits !=L 0L) & (exponent == 0x7FFF) {
      -- is nan, shift in mantissa and set exponent + nan bit
      result = (lower_bits <<L 11L) |L 0x7FF8000000000000L
    } else if exponent > 17406 {
      -- is too big, make it infinity
      result = 0x7FF0000000000000L
    } else if exponent < 15297 { -- equivalent to exponent - f80_exp_bias < -f64_exp_bias - f64_sig_bits
      result = 0L
    } else {
      -- if the exponent is within f64_sig_bits of f64_exp_bias, we can represent
      -- it as a subnormal. Resultant mantissa mantissa will be mantissa shifted
      -- right by f80_exp_bias - f64_exp_bias - exponent. We know that this is at most f64_sig_bits
      -- and the bottom 5 bits of the constant part are 0, so we can just use 0 instead of the constant.
      -- not sure why we use upper_bits here instead of exponent
      %shift = extend ((0 - upper_bits) & 0x3F)
      %subnormalized_mantissa = mantissa >>L %shift
      result = %subnormalized_mantissa >>L 11L
      
      -- round extra precision
      %sticky = extend ((mantissa <<L %shift) ==L 0L)
      rounded_part = %sticky |L (%subnormalized_mantissa &L 0x07FFL) 
      if rounded_part >L 0x0400L {
        result = result +L 1L
      } else if rounded_part ==L 0x0400L {
        result = result +L (result &L 1L)
      }
    }

    result = result |L ((extend upper_bits &L 0x8000L) <<L 48L)
    res = bitcast result
  }
|}

let store_big_float = {|
  fn __store_big_float__(float arg, int dest_ptr) {
    long f abs_f res_mantissa, int res_exp ->
    f = bitcast float->long arg
    abs_f = f &L 0x7FFFFFFFFFFFFFFFL
    -- checks if exponent isn't 0x000 or 0x7FF
    if (abs_f +L 0xFFF0000000000000L) <L 0x7FE0000000000000L {
      -- f is normal
      -- shift mantissa into place and set integer bit
      res_mantissa = (f <<L 11L) |L 0x8000000000000000L
      -- shift exponent and add differencee in bias
      res_exp = trunc (f >>L 52L) + 15360
    } else if abs_f >=L 0x7FF0000000000000L {
      -- f is nan or infinity
      -- shift mantiss into place and set integer bit
      res_mantissa = (f <<L 11L) |L 0x8000000000000000L
      res_exp = 0x7FFF
    } else if abs_f !=L 0L {
      -- f is subnormal
      -- the position of the leading one tells us what to make res_exp
      %leading_zeros = clz long abs_f
      -- since we know 11 < %leading_zeros < 64 this could be %leading_zeros - 11
      %scale = (trunc %leading_zeros + 117) & 0x7F
      res_exp = trunc (abs_f >>L (extend (52 - %scale) &L 0xFFFFL))
      res_exp = res_exp ^ 1
      res_exp = res_exp | (15361 - %scale)
      res_mantissa = (f <<L %leading_zeros) |L 0x8000000000000000L
    } else {
      -- is zero
      res_exp = 0
      res_mantissa = 0L
    }

    store long dest_ptr, res_mantissa
    -- add in sign bit
    res_exp = res_exp | (trunc (f >>L 48L) & 0x8000)
    store:8 i16 dest_ptr, res_exp
  }
|}
