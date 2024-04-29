open! Core

type t = int [@@immediate] [@@deriving sexp, compare, hash, equal]

let none = 0
let sign = 1
let overflow = 2
let carry = 4
let zero = 8
let parity = 16
let ( %| ) l r = l lor r
let ( %& ) set value = set land value
let all = sign %| overflow %| carry %| zero %| parity
