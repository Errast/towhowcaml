type int_ = |
type long_ = |
type float_ = |
type vec_ = |

type _ var_decl =
  | Int : string -> int_ var_decl
  | Long : string -> long_ var_decl
  | Float : string -> float_ var_decl
  | Vec : string -> vec_ var_decl

type _ var
type (_, _) s = |
type z = |


module type HKTList = sig
  type 'a elem
  type 'len t = [] : z t | ( :: ) : 'a elem * 'len t -> ('len, 'a) s t
  type 'a func = { f : 'b. 'b elem -> 'a } [@@unboxed]

  val map_list : 'a func -> 'len t -> 'a list
end

module Params : HKTList with type 'a elem = 'a var_decl
module Args : HKTList with type 'a elem = 'a var
module Vars : HKTList with type 'a elem = 'a var_decl
module Locals : HKTList with type 'a elem = 'a var
module Returns : HKTList with type 'a elem = 'a var_decl
module RetVars : HKTList with type 'a elem = 'a var

type ('size, 'underlying, 'bitcast) tag = private
  | I8 : ([> `I8 ], int_, _) tag
  | I16 : ([> `I16 ], int_, _) tag
  | Int : ([> `Int ], int_, _) tag
  | Long : ([> `Long ], long_, [> `Float ]) tag
  | Float : ([> `Float ], float_, [> `Long ]) tag
  | Vec : ([> `Vec ], vec_, _) tag

type ('size, 'underlying) size_tag =
  ('size, 'underlying, [ `Long | `Float ]) tag

type 'underlying lane_tag =
  ([ `I8 | `I16 | `Int | `Long | `Float ], 'underlying) size_tag

type 'a type_tag = ([ `Int | `Long | `Float | `Vec ], 'a) size_tag
type _ expr
type statement

val fn :
  string ->
  'arg_len Params.t ->
  'ret_len Returns.t ->
  'local_len Vars.t ->
  ('arg_len Args.t ->
  'ret_len RetVars.t ->
  'local_len Locals.t ->
  statement list) ->
  Mir.Func.t

val i8 : ([> `I8 ], int_) size_tag
val i16 : ([> `I16 ], int_) size_tag
val int : ([> `Int ], int_) size_tag
val long : ([> `Long ], long_) size_tag
val float : ([> `Float ], float_) size_tag
val vec : ([> `Vec ], vec_) size_tag
val i : int -> int_ expr
val l : int -> long_ expr
val ll : Int64.t -> long_ expr

type 'a signed_arg = [ `Signed | `Unsigned ] * 'a expr

val s : 'a expr -> 'a signed_arg
val u : 'a expr -> 'a signed_arg
val ( := ) : 't var -> 't expr -> statement
val ( ! ) : 't var -> 't expr
val ( %= ) : string -> 't expr -> statement
val ( !% ) : string -> 't expr

type _ if_cont

val if_ : int_ expr -> statement list -> 'cont if_cont -> 'cont
val end_ : statement if_cont
val else_ : (statement list -> statement if_cont -> statement) if_cont
val elif_ : (int_ expr -> statement list -> 'a if_cont -> 'a) if_cont

type while_

val loop_ : statement list -> while_ -> int_ expr -> statement
val while_ : while_

type 'a bin_op = 'a expr -> 'a expr -> 'a expr
type 'a cmp_op = 'a expr -> 'a signed_arg -> int_ expr

val ( == ) : int_ bin_op
val ( != ) : int_ bin_op
val ( ==: ) : long_ bin_op
val ( !=: ) : long_ bin_op
val ( < ) : int_ cmp_op
val ( <= ) : int_ cmp_op
val ( > ) : int_ cmp_op
val ( >= ) : int_ cmp_op
val ( <: ) : long_ cmp_op
val ( <=: ) : long_ cmp_op
val ( >: ) : long_ cmp_op
val ( >=: ) : long_ cmp_op
val ( + ) : int_ bin_op
val ( - ) : int_ bin_op
val ( * ) : int_ bin_op
val ( +: ) : long_ bin_op
val ( -: ) : long_ bin_op
val ( *: ) : long_ bin_op
val ( lor ) : int_ bin_op
val ( & ) : int_ bin_op
val ( ^ ) : int_ bin_op
val ( << ) : int_ bin_op
val ( |: ) : long_ bin_op
val ( &: ) : long_ bin_op
val ( ^: ) : long_ bin_op
val ( <<: ) : long_ bin_op
val ( >> ) : int_ expr -> int_ signed_arg -> int_ expr
val ( >>: ) : long_ expr -> long_ signed_arg -> long_ expr
val load : ?offset:int -> 't type_tag -> int_ expr -> 't expr

val load_s :
  ?offset:int -> ([ `I8 | `I16 ], int_) size_tag -> int_ expr -> int_ expr

val load_z :
  ?offset:int -> ([ `I8 | `I16 ], int_) size_tag -> int_ expr -> int_ expr

val splat : 't lane_tag -> 't expr -> vec_ expr
val not : int_ expr -> int_ expr
val trunc : long_ expr -> int_ expr
val extend_s : int_ expr -> long_ expr
val extend_z : int_ expr -> long_ expr
val bitcast : ([ `Long | `Float ], 't, 's) tag -> 't expr -> 's expr
val clz : ([ `Int | `Long ], 't) size_tag -> 't expr -> 't expr

type at

val at : at

val store :
  ?offset:int -> (_, 't, _) tag -> 't expr -> at -> int_ expr -> statement

val return : statement
