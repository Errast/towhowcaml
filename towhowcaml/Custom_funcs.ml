let elbrun_code = {|
  fn __int_memset__(int dest val count) {
    int c_mod8 i ->
     if not count go .ret

     c_mod8 = count & 7
     if c_mod8 {
       i = count & 0xFFFFFFF8
     .small_loop:
       dest! = val
       dest = dest + 4
       c_mod8 = c_mod8 - 1
       if c_mod8 go .small_loop
     } else {
       i = count
     }

  .big_loop:
    dest:28! = val
    dest:24! = val
    dest:20! = val
    dest:16! = val
    dest:12! = val
    dest:8! = val
    dest:4! = val
    dest! = val
    dest = dest + 32
    i = i - 8
    if i go .big_loop

    .ret:
  }
|}
