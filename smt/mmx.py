from z3 import *
import operator

Xmm = BitVecSort(128)
Mmx = BitVecSort(64)

s = Solver()

class Test:
    def __init__(self, name):
        self.name = name
    def __enter__(self):
        s.push()
    def __exit__(self, exc_typ, exc_val, exc_tb):
        result = s.check()
        print(f"{self.name}: {result}")
        if result == sat:
            print(s.model())
            print(s.assertions())
        s.pop()

a_16, b_16 = Consts('a16 b16',BitVecSort(16))
bv_sat_sign_sub = Function('bv_sat_sign_sub', BitVecSort(16), BitVecSort(16), BitVecSort(16))
s.add(ForAll([a_16,b_16], bv_sat_sign_sub(a_16,b_16) == If(BVSubNoOverflow(a_16,b_16), If(BVSubNoUnderflow(a_16,b_16,True), a_16-b_16, BitVecVal(0,16)), BitVecVal(0x7FFF, 16))))

def shuf(n,l):
    def g(lhs,rhs):
        return Concat(*[Extract(b*8+7,b*8,lhs) if b < n/8 else Extract((b-n/8)*8+7,(b-n/8)*8,rhs) for b in l])
    return g


def mmx_to_xmm(arg):
    return ZeroExt(64, arg)

def vec_bi_op_equiv(f, g=None):
    g = f if g == None else g
    lhs, rhs = Consts('lhs rhs', Mmx)
    res1, res2 = Consts('res1 res2', Xmm)
    s.add(mmx_to_xmm(f(lhs,rhs)) == res1)
    s.add(g(mmx_to_xmm(lhs), mmx_to_xmm(rhs)) == res2)
    s.add(res1 != res2)

def x_on_y(x,y,f):
    def g(*args):
        arr = [f(*map(lambda v: Extract(i+y-1,i,v),args)) for i in range(0,x,y)]
        arr.reverse()
        return Concat(*arr) if len(arr) > 1 else arr[0]
    return g

def mmx_on_i32(f):
    return x_on_y(64,32,f)

def xmm_on_i32(f):
    return x_on_y(128,32,f)

def mmx_on_i16(f):
    return x_on_y(64,16,f)

def xmm_on_i16(f):
    return x_on_y(128,16,f)

def f64_cmp(f):
    def g(lhs,rhs):
        return If(f(fpBVToFP(lhs,Float64()), fpBVToFP(rhs,Float64())),BitVecVal(2**64-1,64),BitVecVal(0,64))
    return g

def mul_add_i16(lhs,rhs):
    l1 = ZeroExt(16,Extract(15,0,lhs))
    l2 = ZeroExt(16,Extract(31,16,lhs))
    r1 = ZeroExt(16,Extract(15,0,rhs))
    r2 = ZeroExt(16,Extract(31,16,rhs))
    return l1*r1 + l2*r2


basics = {
    "PAND": operator.and_,
    "PXOR": operator.xor,
    "PADDD": (32, operator.add),
    "PSUBD": (32, operator.sub),
    "CMPLTPD": (64, f64_cmp(operator.lt)),
    "POR": operator.or_,
    "PSUBSW": (16, bv_sat_sign_sub) ,
    "PMADDWD": (32, mul_add_i16),
}

for name, f in basics.items():
    with Test(name):
        if callable(f):
            vec_bi_op_equiv(f)
        elif isinstance(f, tuple):
            if len(f) == 2:
                size,f = f
                vec_bi_op_equiv(x_on_y(64,size, f), x_on_y(128,size,f))

def narrow_sign_sat(from_, to):
    top = BitVecVal(2**(to-1)-1,from_)
    bot = BitVecVal(-(2**(to-1)),from_)
    def g(v):
        v = If(from_ > top,top,If(from_ < bot,bot,v))
        return Extract(to-1,0,v)
    return g
def narrow_unsign_sat(from_, to):
    top = BitVecVal(2**(to)-1,from_)
    def g(v):
        v = If(from_ > top,top,v)
        return Extract(to-1,0,v)
    return g

def narrow(x,n,y):
    f = x_on_y(x,n,narrow_sign_sat(n,y))
    return lambda l,r: Concat(f(r),f(l))
def narrowu(x,n,y):
    f = x_on_y(x,n,narrow_unsign_sat(n,y))
    return lambda l,r: Concat(f(r),f(l))

with Test("PACKSSDW"):
    vec_bi_op_equiv(narrow(64,32,16),lambda l,r:shuf(128, [4,4,4,4,4,4,4,4,11,10,9,8,3,2,1,0])(narrow(128,32,16)(l,r),r))
with Test("PACKUSWB"):
    vec_bi_op_equiv(narrow(64,16,8),lambda l,r:shuf(128, [4,4,4,4,4,4,4,4,11,10,9,8,3,2,1,0])(narrow(128,16,8)(l,r),r))
