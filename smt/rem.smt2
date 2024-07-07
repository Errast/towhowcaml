(declare-const a Float32)
(declare-const b Float32)

(declare-const rem Float32)
(assert (= rem (fp.rem a b)))

(declare-const div Float32)
(assert (= div (fp.div roundNearestTiesToEven a b)))

(declare-const trunc Float32)
(assert (= trunc (fp.roundToIntegral roundTowardZero div)))

(declare-const mul Float32)
(assert (= mul (fp.mul roundNearestTiesToEven b trunc)))

(declare-const sub Float32)
(assert (= sub (fp.sub roundNearestTiesToEven a mul)))

(assert (not (= rem sub )))
(check-sat)
(get-model)
