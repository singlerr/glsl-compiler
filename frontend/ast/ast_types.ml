open Base

type loc = Lexing.position

let string_of_loc loc =
  Fmt.str "Line:%d Position:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool

end

module String_id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.(=)
end

module Var_name : ID = String_id
module Function_name : ID = String_id

type modifier = MConst | MVar | MVaring | MIn | MOut

let string_of_modifier = function MConst -> "Const" | MVar -> "Var" | MVaring -> "Varing" | MIn -> "In" | MOut -> "Out"

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

let string_of_type = function
  | TEUInt -> "UInt"
  | TEInt -> "Int"
  | TEVoid -> "Void"
  | TEBool -> "Bool"
  | TEFloat -> "Float"
  | TEDouble -> "Double"
  | TESampler2D -> "Sampler2D"
  | TEMat4 -> "Mat4"
  | TEMat3 -> "Mat3"
  | TEBVec2 -> "BVec2"
  | TEBVec3 -> "BVec3"
  | TEBVec4 -> "BVec4"
  | TEIVec2 -> "IVec2"
  | TEIVec3 -> "IVec3"
  | TEIVec4 -> "IVec4"
  | TEUVec2 -> "UVec2"
  | TEUVec3 -> "UVec3"
  | TEUVec4 -> "UVec4"
  | TEVec2 -> "Vec2"
  | TEVec3 -> "Vec3"
  | TEVec4 -> "Vec4"

type param = 
  | TParam of type_expr * Var_name.t

let get_params_types params =
  List.map ~f:(fun (TParam (param_type, _, _, _)) -> param_type) params

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

let string_of_bin_op = function
  | BinOpPlus -> "+"
  | BinOpMinus -> "-"
  | BinOpMult -> "*"
  | BinOpDiv -> "/"
  | BinOpRem -> "%"
  | BinOpLessThan -> "<"
  | BinOpLessThanEq -> "<="
  | BinOpGreaterThan -> ">"
  | BinOpGreaterThanEq -> ">="
  | BinOpAnd -> "&&"
  | BinOpOr -> "||"
  | BinOpEq -> "=="
  | BinOpNotEq -> "!="

let string_of_un_op = function
  | UnOpNot -> "!"
  | UnOpNeg -> "-"
