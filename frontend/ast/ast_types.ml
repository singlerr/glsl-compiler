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


let string_of_type = function
  | TE_BOOL -> "bool"
  | TE_BVEC2 -> "bvec2"
  | TE_BVEC3 -> "bvec3"
  | TE_BVEC4 -> "bvec4"
  | TE_DMAT2 -> "dmat2"
  | TE_DMAT2X2 -> "dmat2x2"
  | TE_DMAT2X3 -> "dmat2x3"
  | TE_DMAT2X4 -> "dmat2x4"
  | TE_DMAT3 -> "dmat3"
  | TE_DMAT3X2 -> "dmat3x2"
  | TE_DMAT3X3 -> "dmat3x3"
  | TE_DMAT3X4 -> "dmat3x4"
  | TE_DMAT4 -> "dmat4"
  | TE_DMAT4X2 -> "dmat4x2"
  | TE_DMAT4X3 -> "dmat4x3"
  | TE_DMAT4X4 -> "dmat4x4"
  | TE_DOUBLE -> "double"
  | TE_DVEC2 -> "dvec2"
  | TE_DVEC3 -> "dvec3"
  | TE_DVEC4 -> "dvec4"
  | TE_FLOAT -> "float"
  | TE_IIMAGE1D -> "iimage1D"
  | TE_IIMAGE1DARRAY -> "iimage1DArray"
  | TE_IIMAGE2D -> "iimage2D"
  | TE_IIMAGE2DARRAY -> "iimage2DArray"
  | TE_IIMAGE2DMS -> "iimage2DMS"
  | TE_IIMAGE2DMSARRAY -> "iimagd2DMSArray"
  | TE_IIMAGE2DRECT -> "iimage2DRect"
  | TE_IIMAGE3D -> "iimage3D"
  | TE_IIMAGEBUFFER -> "iimageBuffer"
  | TE_IIMAGECUBE -> "iimageCube"
  | TE_IIMAGECUBEARRAY -> "iimageCubeArray"
  | TE_IMAGE1D -> "image1D"
  | TE_IMAGE1DARRAY -> "image1DArray"
  | TE_IMAGE2D -> "image2D"
  | TE_IMAGE2DARRAY -> "image2DArray"
  | TE_IMAGE2DMS -> "image2DMS"
  | TE_IMAGE2DMSARRAY -> "image2DMSArray"
  | TE_IMAGE2DRECT -> "image2DRect"
  | TE_IMAGE3D -> "image3D"
  | TE_IMAGEBUFFER -> "imageBuffer"
  | TE_IMAGECUBE -> "imageCube"
  | TE_IMAGECUBEARRAY -> "imageCubeArray"
  | TE_ISAMPLER1D -> "isampler1D"
  | TE_ISAMPLER1DARRAY -> "isampler1DArray"
  | TE_ISAMPLER2D -> "isampler2D"
  | TE_ISAMPLER2DARRAY -> "isampler2DArray"
  | TE_ISAMPLER2DMS -> "isampler2DMS"
  | TE_ISAMPLER2DMSARRAY -> "isampler2DMSArray"
  | TE_ISAMPLER2DRECT -> "isampler2DRect"
  | TE_ISAMPLER3D -> "isampler3D"
  | TE_ISAMPLERBUFFER -> "isamplerBuffer"
  | TE_ISAMPLERCUBE -> "isamplerCube"
  | TE_ISAMPLERCUBEARRAY -> "isamplerCubeArray"
  | TE_ISUBPASSINPUT -> "isubpassInput"
  | TE_ISUBPASSINPUTMS -> "isubpassInputMS"
  | TE_ITEXTURE1D -> "itexture1D"
  | TE_ITEXTURE1DARRAY -> "itexture1DArray"
  | TE_ITEXTURE2D -> "itexture2D"
  | TE_ITEXTURE2DARRAY -> "itexture2DArray"
  | TE_ITEXTURE2DMS -> "itexture2dMS"
  | TE_ITEXTURE2DMSARRAY -> "itexture2DMSArray"
  | TE_ITEXTURE2DRECT -> "itexture2DRect"
  | TE_ITEXTURE3D -> "itexture3D"
  | TE_ITEXTUREBUFFER -> "itextureBuffer"
  | TE_ITEXTURECUBE -> "itextureCube"
  | TE_ITEXTURECUBEARRAY -> "itextureCubeArray"
  | TE_IVEC2 -> "ivec2"
  | TE_IVEC3 -> "ivec3"
  | TE_IVEC4 -> "ivec4"
  | TE_MAT2 -> "mat2"
  | TE_MAT2X2 -> "mat2x2"
  | TE_MAT2X3 -> "mat2x3"
  | TE_MAT2X4 -> "mat2x4"
  | TE_MAT3 -> "mat3"
  | TE_MAT3X2 -> "mat3x2"
  | TE_MAT3X3 -> "mat3x3"
  | TE_MAT3X4 -> "mat3x4"
  | TE_MAT4 -> "mat4"
  | TE_MAT4X2 -> "mat4x2"
  | TE_MAT4X3 -> "mat4x3"
  | TE_MAT4X4 -> "mat4x4"
  | TE_SAMPLER1D -> "sampler1D"
  | TE_SAMPLER1DARRAY -> "sampler1DArray"
  | TE_SAMPLER1DARRAYSHADOW -> "sampler1DArrayShadow"
  | TE_SAMPLER1DSHADOW -> "sampler1DShadow"
  | TE_SAMPLER2D -> "sampler2D"
  | TE_SAMPLER2DARRAY -> "sampler2DArray"
  | TE_SAMPLER2DARRAYSHADOW -> "sampler2DArrayShadow"
  | TE_SAMPLER2DMS -> "sampler2DMS"
  | TE_SAMPLER2DMSARRAY -> "sampler2DMSArray"
  | TE_SAMPLER2DRECT -> "sampler2DRect"
  | TE_SAMPLER2DRECTSHADOW -> "sampler2DRectShadow"
  | TE_SAMPLER2DSHADOW -> "sampler2DShadow"
  | TE_SAMPLER3D -> "sampler3D"
  | TE_SAMPLERBUFFER -> "samplerBuffer"
  | TE_SAMPLERCUBE -> "samplerCube"
  | TE_SAMPLERCUBEARRAY -> "samplerCubeArray"
  | TE_SAMPLERCUBEARRAYSHADOW -> "samplerCubeArrayShadow"
  | TE_SAMPLERCUBESHADOW -> "samplerCubeShadow"
  | TE_SAMPLERSHADOW -> "samplerShadow"
  | TE_SUBPASSINPUT -> "subpassInput"
  | TE_SUBPASSINPUTMS -> "subpassInputMS"
  | TE_TEXTURE1D -> "texture1D"
  | TE_TEXTURE1DARRAY -> "texture1DArray"
  | TE_TEXTURE2D -> "texture2D"
  | TE_TEXTURE2DARRAY -> "texture2DArray"
  | TE_TEXTURE2DMS -> "texture2DMS"
  | TE_TEXTURE2DMSARRAY -> "texture2DMSArray"
  | TE_TEXTURE2DRECT -> "textur2DRect"
  | TE_TEXTURE3D -> "texture3D"
  | TE_TEXTUREBUFFER -> "textureBuffer"
  | TE_TEXTURECUBE -> "textureCube"
  | TE_TEXTURECUBEARRAY -> "textureCubeArray"
  | TE_UIMAGE1D -> "uimage1D"
  | TE_UIMAGE1DARRAY -> "uimage1DArray"
  | TE_UIIMAGE2D -> "uimage2D"
  | TE_UIMAGE2DARRAY -> "uimage2DArray"
  | TE_UIMAGE2DMS -> "uimage2DMS"
  | TE_UIMAGE2DMSARRAY -> "uimage2DMSArray"
  | TE_UIMAGE2DRECT -> "uimage2DRect"
  | TE_UIMAGE3D -> "uimage3D"
  | TE_UIMAGEBUFFER -> "uimageBuffer"
  | TE_UIMAGECUBE -> "uimageCube"
  | TE_UIMAGECUBEARRAY -> "uimageCubeArray"
  | TE_UINT -> "uint"
  | TE_USAMPLER1D -> "usampler1D"
  | TE_USAMPLER1DARRAY -> "usampler1DArray"
  | TE_USAMPLER2D -> "usampler2D"
  | TE_USAMPLER2DARRAY -> "usampler2DArray"
  | TE_USAMPLER2DMS -> "usampler2DMS"
  | TE_USAMPLER2DMSARRAY -> "usampler2DMSArray"
  | TE_USAMPLER2DRECT -> "usampler2DRect"
  | TE_USAMPLER3D -> "usampler3D"
  | TE_USAMPLERBUFFER -> "usamplerBuffer"
  | TE_USAMPLERCUBE -> "usamplerCube"
  | TE_USAMPLERCUBEARRAY -> "usamplerCubeArray"
  | TE_USUBPASSINPUT -> "usubpassInput"
  | TE_USUBPASSINPUTMS -> "usubpassInputMS"
  | TE_UTEXTURE1D -> "utexture1D"
  | TE_UTEXTURE1DARRAY -> "utexture1DArray"
  | TE_UTEXTURE2D -> "utexture2D"
  | TE_UTEXTURE2DARRAY -> "utexture2DArray"
  | TE_UTEXTURE2DMS -> "utexture2DMS"
  | TE_UTEXTURE2DMSARRAY -> "utexture2DMSArray"
  | TE_UTEXTURE2DRECT -> "utexture2DRect"
  | TE_UTEXTURE3D -> "utexture3D"
  | TE_UTEXTUREBUFFER -> "utextureBuffer"
  | TE_UTEXTURECUBE -> "utextureCube"
  | TE_UTEXTURECUBEARRAY -> "utextureCubeArray"
  | TE_UVEC2 -> "uvec2"
  | TE_UVEC3 -> "uvec3"
  | TE_UVEC4 -> "uvec4"
  | TE_VEC2 -> "vec2"
  | TE_VEC3 -> "vec3"
  | TE_VEC4 -> "vec4"
  | TE_VOID -> "void"


type param = 
  | TParam of type_expr * Var_name.t

let get_params_types params =
  List.map ~f:(fun (TParam (param_type, _)) -> param_type) params

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
  | BinOpXOR -> "^^"
let string_of_un_op = function
  | UnOpNot -> "!"
  | UnOpNeg -> "-"
