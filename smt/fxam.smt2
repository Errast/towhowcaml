(declare-const arg Float64)

(declare-const c0 Bool)
(assert (= c0 (or (fp.isNaN arg) (fp.isInfinite arg) )))

(declare-const c2 Bool)
(assert (= c2 (or (fp.isNormal arg) (fp.isInfinite arg) (fp.isSubnormal arg))))

(declare-const c3 Bool)
(assert (= c3 (or ())))
