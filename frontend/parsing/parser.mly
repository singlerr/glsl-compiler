%{
open Ast.Ast_types
open Parsed_ast
%}

%token <int> INT
%token ATOMIC_UINT
%token ATTRIBUTE
%token BEHAVIOUR
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
%token CONST
%token DEBUG
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
%token DVEC4
%token ELSE
%token ELSE_DIRECTIVE
%token ELIF
%token ENDIF_DIRECTIVE
%token EXTENSION_DIRECTIVE
%token ERROR_MESSAGE
%token ERROR_DIRECTIVE
%token FALSE
%token FLAT
%token FLOAT
%token FOR
%token HIGHP
%token IDENTIFIER
%token IF
%token IF_DIRECTIVE
%token IFDEF_DIRECTIVE
%token IFNDEF_DIRECTIVE
%token IIMAGE1D
%token IIMAGE1DARRAY
%token IIMAGE2D
%token IIMAGE2DMS
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
%token IMAGE2DMSARRAY
%token IMAGE2DMS
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
%token LEFT_PAREN
%token LINE_DIRECTIVE
%token LINE_EXPRESSION
%token LOWP
%token MACRO_ESE_NEWLINE
%token MACRO_IDENTIFIER
%token MACRO_TEXT
%token MACRO_NAME
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
%token NUMBER
%token OFF
%token ON
%token OPTIMIZE
%token OUT
%token PATCH
%token PRAGMA_DIRECTIVE
%token PRECISE
%token PRECISION
%token PROFILE
%token PROGRAM_TEXT
%token READONLY
%token RESTRICT
%token RETURN
%token SAMPLE
%token SAMPLER
%token SAMPLER1D
%token SAMPLER1DARRAY
%token SAMPLER1DARRAYSHADOW
%token SAMPLER1DSHADOW
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
%token SUB_ASSIGN
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
%token UNDEF_DIRECTIVE
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
%token VERSION_DIRECTIVE
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
%token RE_OP
%token RIGHT_ASSIGN
%token RIGHT_BRACE
%token RIGHT_BRACKET
%token RIGHT_OP
%token RIGHT_PAREN
%token SEMICOLON
%token STDGL
%token TILDE
%token VERTICAL_BAR
%token XOR_ASSIGN
%token XOR_OP
%token OR_OP
%token LEFT_ANGLE
%token RIGHT_ANGLE
%token PLUS
%token SLASH
%token STAR
%token PERCENT

%right EQUAL
%left PLUS DASH LEFT_ANGLE RIGHT_ANGLE
%left STAR SLASH PERCENT
%left AND_OP OR_OP XOR_OP
%nonassoc BANG

%start translation_unit
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

constant_expression_directive:
  | CONSTANT_EXPR {}

define_directive:
  | NUMBER_SIGN DEFINE macro_name macro_text {}

elif_directive: 
  | NUMBER_SIGN ELIF constant_expression_directive group_of_lines {}

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

extension_name:
  | EXTENSION_DIRECTIVE {}

group_of_lines:
  | list(group_of_lines_) {}

group_of_lines_:
  | program_text 
  | compiler_directive {}
if_directive:
  | NUMBER_SIGN IF_DIRECTIVE constant_expression_directive group_of_lines elif_directive* else_directive? endif_directive {}

ifdef_directive:
  | NUMBER_SIGN IFDEF_DIRECTIVE macro_identifier group_of_lines elif_directive* else_directive? endif_directive {}

ifndef_directive:
  | NUMBER_SIGN IFNDEF_DIRECTIVE macro_identifier group_of_lines elif_directive* else_directive? endif_directive {}

line_directive:
  | NUMBER_SIGN LINE_DIRECTIVE line_expression {}

line_expression:
  | LINE_EXPRESSION {}

macro_identifier:
  | MACRO_IDENTIFIER {}

macro_esc_newline:
  | MACRO_ESE_NEWLINE {}

macro_name:
  | MACRO_NAME {}

macro_text:
  | list(macro_val) {}

macro_val:
  | macro_text_
  | macro_esc_newline {}

macro_text_:
  | MACRO_TEXT {}

number:
  | NUMBER {}

off:
  | OFF {}

on:
  | ON {}

pragma_optimize:
  | OPTIMIZE LEFT_PAREN pragma_optimize_ RIGHT_PAREN {} 

pragma_optimize_:
  | on
  | off {}

pragma_debug:
  | DEBUG LEFT_PAREN pragma_debug_ RIGHT_PAREN {}

pragma_debug_:
  | on
  | off {}

pragma_directive:
  | NUMBER_SIGN PRAGMA_DIRECTIVE pragma_directive_ {}

pragma_directive_:
  | stdgl
  | pragma_debug
  | pragma_optimize {}

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
  | postfix_expression DOT field_selection
  | postfix_expression INC_OP
  | postfix_expression DEC_OP {}

field_selection:
  | variable_identifier
  | function_call {}

integer_expression:
  | expression {}

function_call:
  | function_identifier LEFT_PAREN function_call_parameters? RIGHT_PAREN {}

function_identifier:
  | type_specifier
  | postfix_expression {}

function_call_parameters:
  | assignment_expression separated_list(COMMA, assignment_expression)
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
  | binary_expression first_prec_op binary_expression
  | binary_expression second_prec_op binary_expression
  | binary_expression comp_op binary_expression
  | binary_expression comp_branch_op binary_expression
  | binary_expression eq_comp_branch_op binary_expression
  | binary_expression AMPERSAND binary_expression
  | binary_expression CARET binary_expression
  | binary_expression VERTICAL_BAR binary_expression
  | binary_expression AND_OP binary_expression
  | binary_expression XOR_OP binary_expression
  | binary_expression OR_OP binary_expression {}

first_prec_op:
  | STAR
  | SLASH
  | PERCENT {}

second_prec_op:
  | PLUS
  | DASH {}

comp_op:
  | LEFT_OP
  | RIGHT_OP {}

comp_branch_op:
  | LEFT_ANGLE
  | RIGHT_ANGLE 
  | LE_OP 
  | RE_OP {}

eq_comp_branch_op:
  | EQ_OP
  | NE_OP {}

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
    option(declaration_) SEMICOLON
  | type_qualifier option(identifier_list) SEMICOLON {}

declaration_:
  | IDENTIFIER option(array_specifier) {}

identifier_list:
  | IDENTIFIER separated_list(COMMA, IDENTIFIER) {}

function_prototype:
  | fully_specified_type IDENTIFIER LEFT_PAREN option(function_parameters) RIGHT_PAREN {}

function_parameters:
  | parameter_declaration separated_list(COMMA, parameter_declaration) {}

parameter_declarator:
  | type_specifier IDENTIFIER option(array_specifier) {}

parameter_declaration:
  | type_qualifier parameter_type_
  | parameter_declarator
  | parameter_type_specifier {}

parameter_type_:
  | parameter_declarator
  | parameter_type_specifier {}

parameter_type_specifier:
  | type_specifier {}

init_declarator_list:
  | single_declaration separated_list(COMMA, typeless_declaration) {}

single_declaration:
  | fully_specified_type option(typeless_declaration) {}

typeless_declaration:
  | IDENTIFIER option(array_specifier) option(typeless_declaration_) {}

typeless_declaration_:
  | EQUAL initializer_ {}

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
  | layout_qualifier_id separated_list(COMMA, layout_qualifier_id) {}

layout_qualifier_id:
  | IDENTIFIER option(layout_qualifier_id_)
  | SHARED {}

layout_qualifier_id_:
  | EQUAL constant_expression {}

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
  | SUBROUTINE option(subroutine_)
  | ATTRIBUTE
  | VARYING {}

subroutine_:
  | LEFT_PAREN type_name_list RIGHT_PAREN {}

type_name_list:
  | type_name separated_list(COMMA, type_name) {}

type_name:
  | IDENTIFIER {}

type_specifier:
  | type_specifier_nonarray option(array_specifier) {}

array_specifier:
  | dimension+ {}

dimension:
  | LEFT_BRACKET option(constant_expression) RIGHT_BRACKET {}

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
    | type_name {}

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
  | struct_declarator separated_list(COMMA, struct_declarator) {}

struct_declarator:
  | IDENTIFIER array_specifier? {}

initializer_:
  | assignment_expression
  | LEFT_BRACE initializer_list COMMA? RIGHT_BRACE {}

initializer_list:
  | initializer_ separated_list(COMMA, initializer_) {}

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
  | statement option(selection_rest_statement_) {}

selection_rest_statement_:
  | ELSE statement {}

condition:
  | expression
  | fully_specified_type IDENTIFIER EQUAL initializer_ {}

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