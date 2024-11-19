{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
        let pos = lexbuf.lex_curr_p
in
        lexbuf.lex_curr_p <-
                { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
                }
}
let unsigned = ['u' 'U']
let hex = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
let decimal = ['1'-'9'] ['0'-'9']*
let octal = '0' ['0'-'7']*

let digit = ['0'-'9']

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = decimal | hex | octal
let uint = int unsigned
let double = digit+ (['e' 'E'] ['+' '-']? digit+)? ['lf' 'Lf']? | ('.' digit+ | digit+ '.' digit?) 
let float = digit+ (['e' 'E'] ['+' '-']? digit+)? ['f' 'F']? | ('.' digit+ | digit+ '.' digit?)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let macro_args = '(' (macro_args | [^ '(' ')']* ')'
let single_line_comment = '//' (^newline)* newline
let block_comment = '/*' (_*)? '*/'

rule read_token = parse
        | "atomic_uint" { ATOMIC_UINT }
        | "attribute" { ATTRIBUTE }
        | "bool" { BOOL }
        | "break" { BREAK }
        | "buffer" { BUFFER }
        | "bvec2" { BVEC2 }
        | "bvec3" { BVEC3 }
        | "bvec4" { BVEC4 }
        | "case" { CASE }
        | "centroid" { CENTROID }
        | "coherent" { COHERENT }
        | "const" { CONST }
        | "continue" { CONTINUE }
        | "default" { DEFAULT }
        | "discard" { DISCARD }
        | "dmat2" { DMAT2 }
        | "dmat2x2" { DMAT2X2 }
        | "dmat2x3" { DMAT2X3 }
        | "dmat2x4" { DMAT2X4 }
        | "dmat3" { DMAT3 }
        | "dmat3x2" { DMAT3X2 }
        | "dmat3x3" { DMAT3X3 }
        | "dmat3x4" { DMAT3X4 }
        | "dmat4" { DMAT4 }
        | "dmat4x2" { DMAT4X2 }
        | "dmat4x3" { DMAT4X3 }
        | "dmat4x4" { DMAT4X4 }
        | "do" { DO }
        | "double" { DOUBLE }
        | "dvec2" { DVEC2 }
        | "dvec3" { DVEC3 }
        | "dvec4" { DVEC4 }
        | "else" { ELSE }
        | "false" { FALSE }
        | "flat" { FLAT }
        | "float" { FLOAT }
        | "for" { FOR }
        | "highp" { HIGHP }
        | "if" { IF }
        | "iimage1D" { IIMAGE1D }
        | "iimage1DArray" { IIMAGE1DARRAY }
        | "iimage2D" { IIMAGE2D }
        | "iimage2DArray" { IIMAGE2DARRAY }
        | "iimage2DMS" { IIMAGE2DMS }
        | "iimage2DMSArray" { IIMAGE2DMSARRAY }
        | "iimage2DRect" { IIMAGE2DRECT }
        | "iimage3D" { IIMAGE3D }
        | "iimageBuffer" { IIMAGEBUFFER }
        | "iimageCube" { IIMAGECUBE }
        | "iimageCubeArray" { IIMAGECUBEARRAY }
        | "image1D" { IMAGE1D }
        | "image1DArray" { IMAGE1DARRAY }
        | "image2D" { IMAGE2D }
        | "image2DArray" { IMAGE2DARRAY }
        | "image2DMS" { IMAGE2DMS }
        | "image2DMSArray" { IMAGE2DMSARRAY }
        | "image2DRect" { IMAGE2DRECT }
        | "image3D" { IMAGE3D }
        | "imageBuffer" { IMAGEBUFFER }
        | "imageCube" { IMAGECUBE }
        | "imageCubeArray" { IMAGECUBEARRAY }
        | "in" { IN }
        | "inout" { INOUT }
        | "int" { INT }
        | "invariant" { INVARIANT }
        | "isample1D" { ISAMPLER1D }
        | "isamer1DArray" { ISAMPLER1DARRAY }
        | "isampler2D" { ISAMPLER2D }
        | "isampler2DArray" { ISAMPLER2DARRAY }
        | "isampler2DMS" { ISAMPLER2DMS }
        | "isampler2DMSArray" { ISAMPLER2DMSARRAY }
        | "isampler2DRect" { ISAMPLER2DRECT }
        | "isampler3D" { ISAMPLER3D }
        | "isamplerBuffer" { ISAMPLERBUFFER }
        | "isamplerCube" { ISAMPLERCUBE }
        | "isamplerCubeArray" { ISAMPLERCUBEARRAY }
        | "isubpassInput" { ISUBPASSINPUT }
        | "isubpassInputMS" { ISUBPASSINPUTMS }
        | "itexture1D" { ITEXTURE1D }
        | "itexture1DArray" { ITEXTURE1DARRAY }
        | "itexture2D" { ITEXTURE2D }
        | "itexture2DArray" { ITEXTURE2DARRAY }
        | "itexture2DMS" { ITEXTURE2DMS }
        | "itexture2DMSArray" { ITEXTURE2DMSARRAY }
        | "itexture2DRect" { ITEXTURE2DRECT }
        | "itexture3D" { ITEXTURE3D }
        | "itextureBuffer" { ITEXTUREBUFFER }
        | "itextureCube" { ITEXTURECUBE }
        | "itextureCubeArray" { ITEXTURECUBEARRAY }
        | "ivec2" { IVEC2 }
        | "ivec3" { IVEC3 }
        | "ivec4" { IVEC4 }
        | "layout" { LAYOUT }
        | "lowp" { LOWP }
        | "mat2" { MAT2 }
        | "mat2x2" { MAT2X2 }
        | "mat2x3" { MAT2X3 }
        | "mat2x4" { MAT2X4 }
        | "mat3" { MAT3 }
        | "mat3x2" { MAT3X2 }
        | "mat3x3" { MAT3X3 }
        | "mat3x4" { MAT3X4 }
        | "mat4" { MAT4 }
        | "mat4x2" { MAT4X2 }
        | "mat4x3" { MAT4X3 }
        | "mat4x4" { MAT4X4 }
        | "mediump" { MEDIUMP }
        | "noperspective" { NOPERSPECTIVE }
        | "out" { OUT }
        | "patch" { PATCH }
        | "precise" { PRECISE }
        | "precision" { PRECISION }
        | "readonly" { READONLY }
        | "restrict" { RESTIRCT }
        | "return" { RETURN }
        | "sample" { SAMPLE }
        | "sampler" { SAMPLER }
        | "sampler1D" { SAMPLER1D }
        | "sampler1DArray" { SAMPLER1DARRAY }
        | "sampler1DArrayShadow" { SAMPLER1DARRAYSHADOW }
        | "sampler2D" { SAMPLER2D }
        | "sampler2DArray" { SAMPLER2DARRAY }
        | "sampler2DArrayShadow" { SAMPLER2DARRAYSHADOW }
        | "sampler2DMS" { SAMPLER2DMS }
        | "sampler2DMSArray" { SAMPLER2DMSARRAY }
        | "sampler2DRect" { SAMPLER2DRECT }
        | "sampler2DRectShadow" { SAMPLER2DRECTSHADOW }
        | "sampler2DShadow" { SAMPLER2DSHADOW }
        | "sampler3D" { SAMPLER3D }
        | "samplerBuffer" { SAMPLERBUFFER }
        | "samplerCube" { SAMPLERCUBE }
        | "samplerCubeArray" { SAMPLERCUBEARRAY }
        | "samplerCubeArrayShadow" { SAMPLERCUBEARRAYSHADOW }
        | "samplerCubeShadow" { SAMPLERCUBESHADOW }
        | "samplerShadow" { SAMPLERSHADOW }
        | "shared" { SHARED }
        | "smooth" { SMOOTH }
        | "struct" { STRUCT }
        | "subpassInput" { SUBPASSINPUT }
        | "subpassInputMS" { SUBPASSINPUTMS }
        | "subroutine" { SUBROUTINE }
        | "switch" { SWITCH }
        | "texture1D" { TEXTURE1D }
        | "texture1DArray" { TEXTURE1DARRAY }
        | "texture2D" { TEXTURE2D }
        | "texture2DArray" { TEXUTRE2DARRAY }
        | "texture2DMS" { TEXTURE2DMS }
        | "texture2DMSArray" { TEXTURE2DMSARRAY }
        | "texture2DRect" { TEXTURE2DRECT }
        | "texture3D" { TEXTURE3D }
        | "textureBuffer" { TEXTUREBUFFER }
        | "textureCube" { TEXTURECUBE }
        | "textureCubeArray" { TEXTURECUBEARRAY }
        | "true" { TRUE }
        | "uimage1D" { UIMAGE1D }
        | "uimage1DArray" { UIMAGE1DARRAY }
        | "uimage2D" { UIMAGE2D }
        | "uimage2DArray" { UIMAGE2DARRAY }
        | "uimage2DMS" { UIMAGE2DMS }
        | "uiamge2DMSArray" { UIMAGE2DMSARRAY }
        | "uiamge2DRect" { UIMAGE2DRECT }
        | "uimage3D" { UIMAGE3D }
        | "uimageBuffer" { UIMAGEBUFFER }
        | "uimageCube" { UIMAGECUBE }
        | "uimageCubeArray" { UIMAGECUBEARRYA }
        | "uint" { UINT }
        | "uniform" { UNIFORM }
        | "usam1D" { USAMPLER1D }
        | "usampler1DArray" { USAMPLER1DARRAY }
        | "usampler2D" { USAMPLER2D }
        | "usampler2DArray" { USAMPLER2DARRAY }
        | "usampler2DMS" { USAMPLER2DMS }
        | "usampler2DMSArray" { USAMPLER2DMSARRAY }
        | "usampler2DRect" { USAMPLER2DRECT }
        | "usampler3D" { USAMPLER3D }
        | "usamplerBuffer" { USAMPLERBUFFER }
        | "usamplerCube" { USAMPLERCUBE }
        | "usamplerCubeArray" { USAMPLERCUBEARRAY }
        | "usubpassInput" { USUBPASSINPUT }
        | "usubpassInputMS" { USUBPASSINPUTMS }
        | "utexture1D" { UTEXTURE1D }
        | "utexture1DArray" { UTEXTURE1DARRAY }
        | "utexture2D" { UTEXTURE2D }
        | "utexture2DArray" { UTEXTURE2DARRAY }
        | "utexture2DMS" { UTEXTURE2DMS }
        | "utexture2DMSArray" { UTEXTURE2DMSARRAY }
        | "utexture2DRect" { UTEXTURE2DRECT }
        | "utexture3D" { UTEXTURE3D }
        | "utextureBuffer" { UTEXTUREBUFFER }
        | "utextureCube" { UTEXTURECUBE }
        | "utextureCubeArray" { UTEXTURECUBEARRAY }
        | "uvec2" { UVEC2 }
        | "uvec3" { UVEC3 }
        | "uvec4" { UVEC4 }
        | "varying" { VARYING }
        | "vec2" { VEC2 }
        | "vec3" { VEC3 }
        | "vec4" { VEC4 }
        | "void" { VOID }
        | "volatile" { VOLATILE }
        | "while" { WHILE }
        | "writeonly" { WRITEONLY }
        | "+=" { ADD_ASSIGN }
        | "&" { AMPERSAND }
        | "&=" { AND_ASSIGN }
        | "&&" { AND_OP }
        | "!" { BANG }
        | "^" { CARET }
        | ";" { COLON }
        | "," { COMMA }
        | "-" { DASH }
        | "--" { DEC_OP }
        | "/=" { DIV_ASSIGN }
        | "." { DOT }
        | "==" { EQ_OP }
        | "=" { EQUAL }
        | ">=" { GE_OP }
        | "++" { INC_OP }
        | "<=" { LE_OP }
        | "<" { LEFT_ANGLE }
        | "<<=" { LEFT_ASSIGN }
        | "{" { LEFT_BRACE }
        | "[" { LEFT_BRACKET }
        | "<<" { LEFT_OP }
        | "(" { LEFT_PAREN }
        | "%" { MOD_ASSIGN }
        | "*=" { MUL_ASSIGN }
        | "!=" { NE_OP }
        | "#" { NUMBER_SIGN } (**TODO: how to process macros?*)
        | "|=" { OR_ASSIGN }
        | "||" { OR_OP }
        | "%" { PERCENT }
        | "+" { PLUS }
        | "?" { QUESTION }
        | ">" { RIGHT_ANGLE }
        | ">>=" { RIGHT_ASSIGN }
        | "}" { RIGHT_BRACE }
        | "]" { RIGHT_BRACKET }
        | ">>" { RIGHT_OP }
        | ")" { RIGHT_PAREN }
        | ";" { SEMICOLON }
        | "/" { SLASH }
        | "*" { STAR }
        | "-=" { SUB_ASSIGN }
        | "~" { TILDE }
        | "|" { VERTICAL_BAR }
        | "^=" { XOR_ASSIGN }
        | "^^" { XOR_OP }
        | double { DOUBLE }
        | float { FLOAT }
        | whitespace { read_token lexbuf }
        | "//" { read_single_line_comment lexbuf }
        | "/*" { read_multi_line_comment lexbuf }
        | int { INT(int_of_string (Lexing.lexeme lexbuf)) }
        | id { ID (Lexing.lexeme lexbuf ) }
        | '"' { read_string(Buffer.create 17) lexbuf }
        | newline { next_line lexbuf; read_token lexbuf }
        | eof { EOF }
        | _ { raise(SyntaxError("Lexing error; Illegal character: " ^ Lexing.lexeme lexbuf)) }
    and read_macro = parse
      | "define" { DEFINE }
      |
    and read_single_line_comment = parse
      | newline { next_line lexbuf; read_token lexbuf }
      | eof { EOF }
      | _ { read_single_line_comment lexbuf }
    and read_multi_line_comment = parse
      | "*/" { read_token lexbuf }
      | newline { next_line lexbuf; read_multi_line_comment lexbuf }
      | eof { raise(SyntaxError("Lexing error; Unexpected EOF")) }
      | _ { read_multi_line_comment lexbuf }
    and read_string buf = parse
      | '"' { STRING(Buffer.contents buf) }
      | '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
      | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
      | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
      | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
      | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
      | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
      | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
      | [^ '"' '\\']+
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
          read_string buf lexbuf
        }
      | _ { raise (SyntaxError("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
      | eof { raise (SyntaxError("String is not terminated")) }

