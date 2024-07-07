(declare-const lhs Float64)
(declare-const rhs Float64)
(declare-const res Bool)

(declare-const c2 Bool)
(assert (= c2 (or (fp.isNaN lhs) (fp.isNaN rhs))))

(declare-const c0 Bool)
(assert (= c0 (or c2 (fp.lt lhs rhs))))

(declare-const c3 Bool)
(assert (= c3 (or c2 (fp.eq lhs rhs))))

(define-fun cf () Bool c0)
(define-fun pf () Bool c2)
(define-fun zf () Bool c3)

(push)
(assert (not (= (not cf) (fp.geq lhs rhs))))
(echo "JAE")
(check-sat)
(pop)

(push)
(assert (not (= (or cf zf) (not (fp.gt lhs rhs)))))
(echo "JBE")
(check-sat)
(pop)

(push)
(assert (not (= pf (or (not (fp.eq lhs lhs)) (not (fp.eq rhs rhs))))))
(echo "JP")
(check-sat)
(pop)

(push)
(assert (not (= (not zf) (or (fp.gt lhs rhs) (fp.lt lhs rhs)))))
(echo "JNE")
(check-sat)
(pop)

(push)
(assert (not (= zf (= (fp.leq lhs rhs) (fp.geq lhs rhs)))))
(echo "JE")
(check-sat)
(pop)
