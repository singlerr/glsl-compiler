%{
open Ast.Ast_types
open Parsed_ast
%}

%token <int> INT
%token ATOMIC_UINT
%token BOOL
%token BREAK
%token BUFFER
%token BVEC2
%token BVEC3
%token BVEC4
%token CASE
%token CENTROID
%token COHERENT
%token CONTINUE
%token DEFAULT
%token DISCARD
%token DMAT2
%token DMAT2X2
%token DMAT2X3
%token DMAT2X4
%token DMAT3
%token DMAT3X2
%token DMAT3X3
%token DMAT3X4
%token DMAT4
%token DMAT4X2
%token DMAT4X3
%token DMAT4X4
%token DO
%token DOUBLE
%token DVEC2
%token DVEC3
%token DEVC4
%token ELSE
%token FALSE
%token FLAT
%token FLOAT
%token FOR
%token HIGHP
%token IF
%token IIMAGE1D
%token IIMAGE1DARRAY
%token IIMAGE2D
%token IIMAGE2DARRAY
%token IIMAGE2DMSARRAY
%token IIMAGE2DRECT
%token IIMAGE3D
%token IIMAGEBUFFER
%token IIMAGECUBE
%token IIMAGECUBEARRAY
%token IMAGE1D
%token IMAGE1DARRAY
%token IMAGE2D
%token IMAGE2DARRAY
%token IMAGE2DRECT
%token IMAGE3D
%token IMAGEBUFFER
%token IMAGECUBE
%token IMAGECUBEARRAY
%token IN
%token INOUT
%token INVARIANT
%token ISAMPLER1D
%token ISAMPLER1DARRAY
%token ISAMPLER2D
%token ISAMPLER2DARRAY
%token ISAMPLER2DMS
%token ISAMPLER2DMSARRAY
%token ISAMPLER2DRECT
%token ISAMPLER3D
%token ISAMPLERBUFFER
%token ISAMPLERCUBE
%token ISAMPLERCUBEARRAY
%token ISUBPASSINPUT
%token ISUBPASSINPUTMS
%token ITEXTURE1D
%token ITEXTURE1DARRAY
%token ITEXTURE2D
%token ITEXTURE2DARRAY
%token ITEXTURE2DMS
%token ITEXTURE2DMSARRAY
%token ITEXTURE2DRECT
%token ITEXTURE3D
%token ITEXTUREBUFFER
%token ITEXTURECUBE
%token ITEXTURECUBEARRAY
%token IVEC2
%token IVEC3
%token IVEC4
%token LAYOUT
%token LOWP
%token MAT2
%token MAT2X2
%token MAT2X3
%token MAT2X4
%token MAT3
%token MAT3X2
%token MAT3X3
%token MAT3X4
%token MAT4
%token MAT4X2
%token MAT4X3
%token MAT4X4
%token MEDIUMP
%token NOPERSPECTIVE
%token OUT
%token PATCH
%token PRECISE
%token PRECISION
%token READONLY
%token RESTRICT
%token RETURN
%token SAMPLE
%token SAMPLER
%token SAMPLER1D
%token SAMPLER1DARRAY
%token SAMPLER1DARRAYSHADOW
%token SAMPLER2D
%token SAMPLER2DARRAY
%token SAMPLER2DARRAYSHADOW
%token SAMPLER2DMS
%token SAMPLER2DMSARRAY
%token SAMPLER2DRECT
%token SAMPLER2DRECTSHADOW
%token SAMPLER2DSHADOW
%token SAMPLER3D
%token SAMPLERBUFFER
%token SAMPLERCUBE
%token SAMPLERCUBEARRAY
%token SAMPLERCUBEARRAYSHADOW
%token SAMPLERCUBESHADOW
%token SAMPLERSHADOW
%token SHARED
%token SMOOTH
%token STRUCT
%token SUBPASSINPUT
%token SUBPASSINPUTMS
%token SUBROUTINE
%token SWITCH
%token TEXTURE1D
%token TEXTURE1DARRAY
%token TEXTURE2D
%token TEXTURE2DARRAY
%token TEXTURE2DMS
%token TEXTURE2DMSARRAY
%token TEXTURE2DRECT
%token TEXTURE3D
%token TEXTUREBUFFER
%token TEXTURECUBE
%token TEXTURECUBEARRAY
%token TRUE
%token UIMAGE1D
%token UIMAGE1DARRAY
%token UIMAGE2D
%token UIMAGE2DARRAY
%token UIMAGE2DMS
%token UIMAGE2DMSARRAY
%token UIMAGE2DRECT
%token UIMAGE3D
%token UIMAGEBUFFER
%token UIMAGECUBE
%token UIMAGECUBEARRAY
%token UIMAGE2DRECT
%token UINT
%token UNIFORM
%token USAMPLER1D
%token USAMPLER1DARRAY
%token USAMPLER2D
%token USAMPLER2DARRAY
%token USAMPLER2DMS
%token USAMPLER2DMSARRAY
%token USAMPLER2DRECT
%token USAMPLER3D
%token USAMPLERBUFFER
%token USAMPLERCUBE
%token USAMPLERCUBEARRAY
%token USUBPASSINPUT
%token USUBPASSINPUTMS
%token UTEXTURE1D
%token UTEXTURE1DARRAY
%token UTEXTURE2D
%token UTEXTURE2DARRAY
%token UTEXTURE2DMS
%token UTEXTURE2DMSARRAY
%token UTEXTURE2DRECT
%token UTEXTURE3D
%token UTEXTUREBUFFER
%token UTEXTURECUBE
%token UTEXTURECUBEARRAY
%token UVEC2
%token UVEC3
%token UVEC4
%token UVEC4
%token VARYING
%token VEC2
%token VEC3
%token VEC4
%token VOID
%token VOLATILE
%token WHILE
%token WRITEONLY
%token ADD_ASSIGN
%token AMPERSAND
%token AND_ASSIGN
%token AND_OP
%token BANG
%token CARET
%token COLON
%token COMMA
%token DASH
%token DEC_OP
%token DIV_ASSIGN
%token DOT
%token EQ_OP
%token EQUAL
%token GE_OP
%token INC_OP
%token LE_OP
%token LEFT_ASSIGN
%token LEFT_BRACE
%token LEFT_BRACKET
%token LEFT_OP
%token LEFT_PARENT
%token MOD_ASSIGN
%token MUL_ASSIGN
%token NE_OP
%token NUMBER_SIGN
%token OR_ASSIGN
%token QUESTION
%token RIGHT_ASSIGN
%token RIGHT_BRACE
%token RIGHT_BRACKET
%token RIGHT_OP
%token RIGHT_PAREN
%token SEMICOLON
%token TILDE
%token VERTICAL_BAR
%token XOR_ASSIGN

%right EQUAL
%left PLUS DASH LEFT_ANGLE RIGHT_ANGLE
%left STAR SLASH PERCENT
%left AND_OP OR_OP XOR_OP
%nonassoc BANG

%%

translation_unit:
  | compiler_directive = compiler_directive*

compiler_directive:
  | define_directive
  | elif_directive
  | else_directive
  | endif_directive
  | error_directive
  | extension_directive
  | if_directive
  | ifdef_directive
  | ifndef_directive
  | line_directive
  | pragma_directive
  | undef_directive
  | version_directive

behaviour:
  | BEHAVIOUR

constant_expression:
  | CONSTANT_EXPR

define_directive:
  | NUMBER_SIGN DEFINE macro_name macro_text

elif_directive:
  | NUMBER_SIGN ELIF constant_expression group_of_lines

else_directive:
  | NUMBER_SIGN ELSE_DIRECTIVE group_of_lines

endif_directive:
  | NUMBER_SIGN ENDIF_DIRECTIVE

error_directive:
  | NUMBER_SIGN ERROR_DIRECTIVE error_message

error_message:
  | ERROR_MESSAGE

extension_directive:
  | NUMBER_SIGN EXTENSION_DIRECTIVE extension_name COLON behaviour
  