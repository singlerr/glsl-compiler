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
%token CONSTANT_EXPR
%token DEFAULT
%token DEFINE
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
%token ELIF
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

// ---------------------------------- preprocess
translation_unit:
  | compiler_directive {}

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
  | version_directive {}


behaviour:
  | BEHAVIOUR {}

constant_expression:
  | CONSTANT_EXPR {}

define_directive:
  | NUMBER_SIGN DEFINE macro_name macro_text {}

elif_directive: 
  | NUMBER_SIGN ELIF constant_expression group_of_lines {}

else_directive:
  | NUMBER_SIGN ELSE_DIRECTIVE group_of_lines {}

endif_directive:
  | NUMBER_SIGN ENDIF_DIRECTIVE {}

error_directive:
  | NUMBER_SIGN ERROR_DIRECTIVE error_message {}

error_message:
  | ERROR_MESSAGE {}
 
extension_directive:
  | NUMBER_SIGN EXTENSION_DIRECTIVE extension_name COLON behaviour {}

group_of_lines:
  | (program_text | compiler_directive)*

if_directive:
  | NUMBER_SIGN IF_DIRECTIVE constant_expression group_of_lines elif_directive* else_directive? endif_directive {}

ifdef_directive:
  | NUMBER_SIGN IFDEF_DIRECTIVE macro_identifier group_of_lines elif_directive* else_directive? endif_directive {}

ifndef_directive:
  | NUMBER_SIGN IFNDEF_DIRECTIVE macro_identifier group_of_lines elif_directive* else_directive? endif_directive {}

line_directive:
  | NUMBER_SIGN LINE_DIRECTIVE line_expression {}

macro_esc_newline:
  | MACRO_ESE_NEWLINE {}

macro_name:
  | MACRO_NAME {}

macro_text:
  | (macro_text_ | macro_esc_newline)* {}

macro_text_:
  | MACRO_TEXT {}

number:
  | NUMBER {}

off:
  | OFF {}

on:
  | ON {}

pragma_debug:
  | DEBUG LEFT_PAREN (on | off) RIGHT_PAREN {}

pragma_directive:
  | NUMBER_SIGN PRAGMA_DIRECTIVE (stdgl | pragma_debug | pragma_optimize) {}

profile:
  | PROFILE {}

program_text:
  | PROGRAM_TEXT {}

stdgl:
  | STDGL {}

undef_directive:
  | NUMBER_SIGN UNDEF_DIRECTIVE macro_identifier {}

version_directive:
  | NUMBER_SIGN VERSION_DIRECTIVE number profile? {}


//-------------------------------------------------------------------------

variable_identifier:
  | IDENTIFIER {}

primary_expression:
  | variable_identifier
  | TRUE
  | FALSE
  | INT
  | UINT
  | FLOAT
  | DOUBLE
  | LEFT_PAREN expression RIGHT_PAREN {}

postfix_expression:
  | primary_expression
  | postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET
  | postfix_expression LEFT_PAREN function_call_parameters? RIGHT_PAREN
  | type_specifier LEFT_PAREN function_call_parameters? RIGHT_PAREN
  | postfix_expression DOT field_expression
  | postfix_expression INC_OP
  | postfix_expression DEC_OP {}

field_selection:
  | variable_identifier
  | function_call {}

function_identifier:
  | type_specifier
  | postfix_expression {}

function_call_parameters:
  | assignment_expression (COMMA assignment_expression)*
  | VOID {}

unary_expression:
  | postfix_expression
  | INC_OP unary_expression
  | DEC_OP unary_expression
  | unary_operator unary_expression {}

unary_operator:
  | PLUS
  | DASH
  | BANG
  | TILDE {}

assignment_expression:
  | constant_expression
  | unary_expression assignment_operator assignment_expression {}

assignment_operator:
  | EQUAL
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | ADD_ASSIGN
  | SUB_ASSIGN
  | LEFT_ASSIGN
  | RIGHT_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | OR_ASSIGN {}

binary_expression:
  | unary_expression
  | binary_expression (STAR | SLASH | PERCENT) binary_expression
  | binary_expression (PLUS | DASH) binary_expression
  | binary_expression (LEFT_OP | RIGHT_OP) binary_expression
  | binary_expression (LEFT_ANGLE | RIGHT_ANGLE | LE_OP | RE_OP) binary_expression
  | binary_expression (EQ_OP | NE_OP) binary_expression
  | binary_expression AMPERSAND binary_expression
  | binary_expression CARET binary_expression
  | binary_expression VERTICAL_BAR binary_expression
  | binary_expression AND_OP binary_expression
  | binary_expression XOR_OP binary_expression
  | binary_expression OR_OP binary_expression {}

expression:
  | assignment_expression
  | expression COMMA assignment_expression {}

constant_expression:
  | binary_expression
  | binary_expression QUESTION expression COLON assignment_expression {}

declaration:
  | function_prototype SEMICOLON
  | init_declarator_list SEMICOLON
  | PRECISION precision_qualifier type_specifier SEMICOLON
  | type_qualifier IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE
    (IDENTIFIER array_specifier?)? SEMICOLON
  | type_qualifier identifier_list? SEMICOLON {}

identifier_list:
  : IDENTIFIER (COMMA IDENTIFIER)* {}

function_prototype:
  | fully_specified_type IDENTIFIER LEFT_PAREN function_parameters? RIGHT_PAREN {}

function_parameters:
  | parameter_declaration (COMMA parameter_declaration)* {}

parameter_declarator:
  | type_specifier IDENTIFIER array_specifier? {}

parameter_declaration:
  | type_qualifier (parameter_declarator | parameter_type_specifier)
  | parameter_declarator
  | parameter_type_specifier {}

parameter_type_specifier:
  | type_specifier {}

init_declarator_list:
  | single_declaration (COMMA typeless_declaration)* {}

single_declaration:
  | fully_specified_type typeless_declaration? {}

typeless_declaration:
  | IDENTIFIER array_specifier? (EQUAL initializer)? {}

fully_specified_type:
  | type_specifier
  | type_qualifier type_specifier {}

invariant_qualifier:
  | INVARIANT {}

interpolation_qualifier:
  | SMOOTH
  | FLAT
  | NOPERSPECTIVE {}

layout_qualifier:
  | LAYOUT LEFT_PAREN layout_qualifier_id_list RIGHT_PAREN {}

layout_qualifier_id_list:
  | layout_qualifier_id (COMMA layout_qualifier_id)* {}

layout_qualifier_id:
  | IDENTIFIER (EQUAL constant_expression)?
  | SHARED {}

precise_qualifier:
  | PRECISE {}

type_qualifier:
  | single_type_qualifier+ {}

single_type_qualifier:
  | storage_qualifier
  | layout_qualifier
  | precision_qualifier
  | interpolation_qualifier
  | invariant_qualifier
  | precise_qualifier {}

storage_qualifier:
  | CONST
  | IN
  | OUT
  | INOUT
  | CENTROID
  | PATCH
  | SAMPLE
  | UNIFORM
  | BUFFER
  | SHARED
  | COHERENT
  | VOLATILE
  | RESTRICT
  | READONLY
  | WRITEONLY
  | SUBROUTINE (LEFT_PAREN type_name_list RIGHT_PAREN)?
  | ATTRIBUTE
  | VARING {}

type_name_list:
  | type_name (COMMA type_name)* {}

type_name:
  | IDENTIFIER {}

type_specifier:
  | type_specifier_nonarray array_specifier? {}

array_specifier:
  | dimension+ {}

dimension:
  | LEFT_BRACKET constant_expression? RIGHT_BRACKET {}

type_specifier_nonarray:
    | VOID
    | FLOAT
    | DOUBLE
    | INT
    | UINT
    | BOOL
    | VEC2
    | VEC3
    | VEC4
    | DVEC2
    | DVEC3
    | DVEC4
    | BVEC2
    | BVEC3
    | BVEC4
    | IVEC2
    | IVEC3
    | IVEC4
    | UVEC2
    | UVEC3
    | UVEC4
    | MAT2
    | MAT3
    | MAT4
    | MAT2X2
    | MAT2X3
    | MAT2X4
    | MAT3X2
    | MAT3X3
    | MAT3X4
    | MAT4X2
    | MAT4X3
    | MAT4X4
    | DMAT2
    | DMAT3
    | DMAT4
    | DMAT2X2
    | DMAT2X3
    | DMAT2X4
    | DMAT3X2
    | DMAT3X3
    | DMAT3X4
    | DMAT4X2
    | DMAT4X3
    | DMAT4X4
    | ATOMIC_UINT
    | SAMPLER2D
    | SAMPLER3D
    | SAMPLERCUBE
    | SAMPLER2DSHADOW
    | SAMPLERCUBESHADOW
    | SAMPLER2DARRAY
    | SAMPLER2DARRAYSHADOW
    | SAMPLERCUBEARRAY
    | SAMPLERCUBEARRAYSHADOW
    | ISAMPLER2D
    | ISAMPLER3D
    | ISAMPLERCUBE
    | ISAMPLER2DARRAY
    | ISAMPLERCUBEARRAY
    | USAMPLER2D
    | USAMPLER3D
    | USAMPLERCUBE
    | USAMPLER2DARRAY
    | USAMPLERCUBEARRAY
    | SAMPLER1D
    | SAMPLER1DSHADOW
    | SAMPLER1DARRAY
    | SAMPLER1DARRAYSHADOW
    | ISAMPLER1D
    | ISAMPLER1DARRAY
    | USAMPLER1D
    | USAMPLER1DARRAY
    | SAMPLER2DRECT
    | SAMPLER2DRECTSHADOW
    | ISAMPLER2DRECT
    | USAMPLER2DRECT
    | SAMPLERBUFFER
    | ISAMPLERBUFFER
    | USAMPLERBUFFER
    | SAMPLER2DMS
    | ISAMPLER2DMS
    | USAMPLER2DMS
    | SAMPLER2DMSARRAY
    | ISAMPLER2DMSARRAY
    | USAMPLER2DMSARRAY
    | IMAGE2D
    | IIMAGE2D
    | UIMAGE2D
    | IMAGE3D
    | IIMAGE3D
    | UIMAGE3D
    | IMAGECUBE
    | IIMAGECUBE
    | UIMAGECUBE
    | IMAGEBUFFER
    | IIMAGEBUFFER
    | UIMAGEBUFFER
    | IMAGE1D
    | IIMAGE1D
    | UIMAGE1D
    | IMAGE1DARRAY
    | IIMAGE1DARRAY
    | UIMAGE1DARRAY
    | IMAGE2DRECT
    | IIMAGE2DRECT
    | UIMAGE2DRECT
    | IMAGE2DARRAY
    | IIMAGE2DARRAY
    | UIMAGE2DARRAY
    | IMAGECUBEARRAY
    | IIMAGECUBEARRAY
    | UIMAGECUBEARRAY
    | IMAGE2DMS
    | IIMAGE2DMS
    | UIMAGE2DMS
    | IMAGE2DMSARRAY
    | IIMAGE2DMSARRAY
    | UIMAGE2DMSARRAY
    | struct_specifier
    | type_name

precision_qualifier:
  | HIGHP
  | MEDIUMP
  | LOWP {}

struct_specifier:
  | STRUCT IDENTIFIER? LEFT_BRACE struct_declaration_list RIGHT_BRACE {}

struct_declaration_list:
  | struct_declaration+ {}

struct_declaration:
  | type_specifier struct_declarator_list SEMICOLON
  | type_qualifier type_specifier struct_declarator_list SEMICOLON {}

struct_declarator_list:
  | struct_declarator (COMMA struct_declarator)* {}

struct_declarator:
  | IDENTIFIER array_specifier? {}

initializer:
  | assignment_expression
  | LEFT_BRACE initializer_list COMMA? RIGHT_BRACE {}

declaration_statement:
  | declaration {}

statement:
  | compound_statement
  | simple_statement {}

simple_statement:
  | declaration_statement
  | expression_statement
  | selection_statement
  | switch_statement
  | case_label
  | iteration_statement
  | jump_statement {}

compound_statement:
  | LEFT_BRACE statement_list? RIGHT_BRACE {}

statement_no_new_scope:
  | compound_statement_no_new_scope
  | simple_statement {}

compound_statement_no_new_scope:
  | LEFT_BRACE statement_list? RIGHT_BRACE {}

statement_list:
  | statement+ {}

expression_statement:
  | SEMICOLON
  | expression SEMICOLON {}

selection_statement:
  | IF LEFT_PAREN expression RIGHT_PAREN selection_rest_statement {}

selection_rest_statement:
  | statement (ELSE statement)? {}

condition:
  | expression
  | fully_specified_type IDENTIFIER EQUAL initializer {}

switch_statement:
  | SWITCH LEFT_PAREN expression RIGHT_PAREN LEFT_BRACE statement_list? RIGHT_BRACE {}

case_label:
  | CASE expression COLON
  | DEFAULT COLON {}

iteration_statement:
  | WHILE  LEFT_PAREN condition RIGHT_PAREN statement_no_new_scope
  | DO statement WHILE LEFT_PAREN for_init_statement for_rest_statement RIGHT_PAREN statement_no_new_scope {}

for_init_statement:
  | expression_statement
  | declaration_statement {}

for_rest_statement:
  | condition? SEMICOLON expression? {}

jump_statement:
  | CONTINUE SEMICOLON
  | BREAK SEMICOLON
  | RETURN expression? SEMICOLON
  | DISCARD SEMICOLON {}

external_declaration:
  | function_definition
  | declaration
  | SEMICOLON {}

function_definition:
  | function_prototype compound_statement_no_new_scope {}