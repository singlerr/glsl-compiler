type loc = Lexing.position

module type ID = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val ( = ) : t -> t -> bool
end

module Var_name : ID
module Function_name : ID

type type_expr =
  | TE_BOOL
  | TE_BVEC2
  | TE_BVEC3
  | TE_BVEC4
  | TE_DMAT2
  | TE_DMAT2X2
  | TE_DMAT2X3
  | TE_DMAT2X4
  | TE_DMAT3
  | TE_DMAT3X2
  | TE_DMAT3X3
  | TE_DMAT3X4
  | TE_DMAT4
  | TE_DMAT4X2
  | TE_DMAT4X3
  | TE_DMAT4X4
  | TE_DOUBLE
  | TE_DVEC2
  | TE_DVEC3
  | TE_DVEC4
  | TE_FLOAT
  | TE_IIMAGE1D
  | TE_IIMAGE1DARRAY
  | TE_IIMAGE2D
  | TE_IIMAGE2DARRAY
  | TE_IIMAGE2DMS
  | TE_IIMAGE2DMSARRAY
  | TE_IIMAGE2DRECT
  | TE_IIMAGE3D
  | TE_IIMAGEBUFFER
  | TE_IIMAGECUBE
  | TE_IIMAGECUBEARRAY
  | TE_IMAGE1D
  | TE_IMAGE1DARRAY
  | TE_IMAGE2D
  | TE_IMAGE2DARRAY
  | TE_IMAGE2DMS
  | TE_IMAGE2DMSARRAY
  | TE_IMAGE2DRECT
  | TE_IMAGE3D
  | TE_IMAGEBUFFER
  | TE_IMAGECUBE
  | TE_IMAGECUBEARRAY
  | TE_ISAMPLER1D
  | TE_ISAMPLER1DARRAY
  | TE_ISAMPLER2D
  | TE_ISAMPLER2DARRAY
  | TE_ISAMPLER2DMS
  | TE_ISAMPLER2DMSARRAY
  | TE_ISAMPLER2DRECT
  | TE_ISAMPLER3D
  | TE_ISAMPLERBUFFER
  | TE_ISAMPLERCUBE
  | TE_ISAMPLERCUBEARRAY
  | TE_ISUBPASSINPUT
  | TE_ISUBPASSINPUTMS
  | TE_ITEXTURE1D
  | TE_ITEXTURE1DARRAY
  | TE_ITEXTURE2D
  | TE_ITEXTURE2DARRAY
  | TE_ITEXTURE2DMS
  | TE_ITEXTURE2DMSARRAY
  | TE_ITEXTURE2DRECT
  | TE_ITEXTURE3D
  | TE_ITEXTUREBUFFER
  | TE_ITEXTURECUBE
  | TE_ITEXTURECUBEARRAY
  | TE_IVEC2
  | TE_IVEC3
  | TE_IVEC4
  | TE_MAT2
  | TE_MAT2X2
  | TE_MAT2X3
  | TE_MAT2X4
  | TE_MAT3
  | TE_MAT3X2
  | TE_MAT3X3
  | TE_MAT3X4
  | TE_MAT4
  | TE_MAT4X2
  | TE_MAT4X3
  | TE_MAT4X4
  | TE_SAMPLER1D
  | TE_SAMPLER1DARRAY
  | TE_SAMPLER1DARRAYSHADOW
  | TE_SAMPLER1DSHADOW
  | TE_SAMPLER2D
  | TE_SAMPLER2DARRAY
  | TE_SAMPLER2DARRAYSHADOW
  | TE_SAMPLER2DMS
  | TE_SAMPLER2DMSARRAY
  | TE_SAMPLER2DRECT
  | TE_SAMPLER2DRECTSHADOW
  | TE_SAMPLER2DSHADOW
  | TE_SAMPLER3D
  | TE_SAMPLERBUFFER
  | TE_SAMPLERCUBE
  | TE_SAMPLERCUBEARRAY
  | TE_SAMPLERCUBEARRAYSHADOW
  | TE_SAMPLERCUBESHADOW
  | TE_SAMPLERSHADOW
  | TE_SUBPASSINPUT
  | TE_SUBPASSINPUTMS
  | TE_TEXTURE1D
  | TE_TEXTURE1DARRAY
  | TE_TEXTURE2D
  | TE_TEXTURE2DARRAY
  | TE_TEXTURE2DMS
  | TE_TEXTURE2DMSARRAY
  | TE_TEXTURE2DRECT
  | TE_TEXTURE3D
  | TE_TEXTUREBUFFER
  | TE_TEXTURECUBE
  | TE_TEXTURECUBEARRAY
  | TE_UIMAGE1D
  | TE_UIMAGE1DARRAY
  | TE_UIIMAGE2D
  | TE_UIMAGE2DARRAY
  | TE_UIMAGE2DMS
  | TE_UIMAGE2DMSARRAY
  | TE_UIMAGE2DRECT
  | TE_UIMAGE3D
  | TE_UIMAGEBUFFER
  | TE_UIMAGECUBE
  | TE_UIMAGECUBEARRAY
  | TE_UINT
  | TE_USAMPLER1D
  | TE_USAMPLER1DARRAY
  | TE_USAMPLER2D
  | TE_USAMPLER2DARRAY
  | TE_USAMPLER2DMS
  | TE_USAMPLER2DMSARRAY
  | TE_USAMPLER2DRECT
  | TE_USAMPLER3D
  | TE_USAMPLERBUFFER
  | TE_USAMPLERCUBE
  | TE_USAMPLERCUBEARRAY
  | TE_USUBPASSINPUT
  | TE_USUBPASSINPUTMS
  | TE_UTEXTURE1D
  | TE_UTEXTURE1DARRAY
  | TE_UTEXTURE2D
  | TE_UTEXTURE2DARRAY
  | TE_UTEXTURE2DMS
  | TE_UTEXTURE2DMSARRAY
  | TE_UTEXTURE2DRECT
  | TE_UTEXTURE3D
  | TE_UTEXTUREBUFFER
  | TE_UTEXTURECUBE
  | TE_UTEXTURECUBEARRAY
  | TE_UVEC2
  | TE_UVEC3
  | TE_UVEC4
  | TE_VEC2
  | TE_VEC3
  | TE_VEC4
  | TE_VOID
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
  | BinOpXOR

type un_op = UnOpNot | UnOpNeg

val string_of_loc : loc -> string
val string_of_type : type_expr -> string
val string_of_modifier : modifier -> string
val string_of_bin_op : bin_op -> string
val string_of_un_op : un_op -> string
