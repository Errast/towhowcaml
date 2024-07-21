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

  fn __int_diff__(int src dest count) -> (int s2 d2 c) {
    int src_end dest_end ->
    if not count {
      s2 = src
      d2 = dest
      c = 0
      return
    }

    %byte_count = count << 2
    src_end = src + %byte_count
    dest_end = dest + %byte_count

    loop {
      if load src != load dest {
        s2 = src
        d2 = dest
        c = count
        return
      }
      src = src + 4
      dest = dest + 4
      count = count - 1
    } while count

    s2 = src_end
    d2 = dest_end
    c = 0
  }

  fn __byte_diff__(int src dest count) -> (int s2 d2 c) {
    int src_end dest_end ->
    if not count {
      s2 = src
      d2 = dest
      c = 0
      return
    }

    src_end = src + count
    dest_end = dest+count

    loop {
      if load i8 src != load i8 dest { 
        s2 = src
        d2 = dest
        c = count
        return
      }

      src = src + 1
      dest = dest + 1
      count = count - 1
    } while count
    
    s2 = src_end
    d2 = dest_end
    c = 0
  }
|}

let load_big_float = {|
  fn __load_bit_float__(int float_ptr) -> (float res) {
    long lower_bits upper_bits significand, int upper_int exponent, long result ->
    lower_bits = load long float_ptr
    upper_bits = extend load:8 i16 float_ptr
    significand = lower_bits &L 0x7FFFFFFFFFFFFFFFL

    upper_bits = upper_bits &L 0xFFFFL
    upper_int = trunc upper_bits
    exponent = upper_int & 0x7FFF
    -- 15361 = 16383 - 1023 + 1
    %bot_bound = (exponent - 15361) & 0xFFFF
    -- 17407 = 16383 + 2048 - 1023
    %top_bound = (exponent - 17407) & 0xFFFF
    
    -- checks if exponent in range for f64
    -- equivalent to exponent - f80_exp_bias <= f64_exp_bias && exponent - f80_exp_bias <= -f64_exp_bias + 1 
    if %bot_bound < %top_bound {
      -- shift exp and sign into place
      %exponent = upper_bits <<L 52L
      %sign_bits = significand >>L 11L
      result = %exponent |L %sign_bits

      %rounded_part = lower_bits &L 0x07FFL
      if %rounded_part >L 0x000L {
        lower_bits = result +L 0x4000000000000001L
      } else {
        
      }
    }
  }
|}
