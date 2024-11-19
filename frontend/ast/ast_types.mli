type loc = Lexing.position

module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module Var_name : ID
module Function_name : ID

type modifier = MConst | MVar | MVaring (** varing *) | MIn (** in *) | MOut (** out *)
type type_expr =
  | TEUInt
  | TEInt
  | TEVoid
  | TEBool
  | TEFloat
  | TEDouble
  | TESampler2D
  | TEMat4
  | TEMat3
  | TEBVec2
  | TEBVec3
  | TEBVec4
  | TEIVec2
  | TEIVec3
  | TEIVec4
  | TEUVec2
  | TEDVec2
  | TEDVec3
  | TEDVec4
  | TEVec2
  | TEVec3
  | TEVec4
type param =
  | TParam of type_expr * Var_name.t

val get_params_types : param list -> type_expr list


type bin_op = 
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv
  | BinOpRem
  | BinOpLessThan
  | BinOpLessThanEq
  | BinOpGreaterThan
  | BinOpGreaterThanEq
  | BinOpAnd
  | BinOpOr
  | BinOpEq
  | BinOpNotEq

type un_op = UnOpNot | UnOpNeg

val string_of_loc : loc -> string
val string_of_type : type_expr -> string
val string_of_modifier : modifier -> string
val string_of_bin_op : bin_op -> string
val string_of_un_op : un_op -> string
