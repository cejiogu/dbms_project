%{
open Ast
open Final_project
%}

%token <int> INT_ACT
%token <string> ID
// %token <string> STR
%token <float> FLOAT_ACT
%token <string> DATE_ACT
%token CREATE
%token TABLE
%token SCHEMA
%token INSERT
%token INTO
%token VALUES
%token SELECT
%token FROM
%token WHERE
%token EQUALS
%token ALTER
%token ADD
%token INT
%token STRING
%token BOOL
%token FLOAT
%token DATE
%token LPAREN
%token RPAREN
%token COMMA
%token EOF


%start <Ast.expr> prog

%%

prog:
	| CREATE TABLE id=ID LPAREN cols=col_def RPAREN EOF { let names,types=List.split (List.map (fun (n,t) -> (n,Column.elemtype_of_stringparse t)) cols) in CreateTable (Table.make id names types)}
	| SCHEMA {Schema}
	| SELECT col_nm=col_nm_def FROM id=ID EOF {Select (col_nm,id)}
	| ALTER TABLE id=ID ADD nm=ID typ=col_type EOF {AlterTable (id,nm,typ)}
	| INSERT INTO tab_name=ID LPAREN col_nm=col_nm_def RPAREN VALUES LPAREN val_lst=row_vals RPAREN EOF {InsertInto (tab_name,col_nm,val_lst)}
	| SELECT LPAREN col_nm=col_nm_def RPAREN FROM id=ID WHERE col=ID EQUALS e_valu=data EOF {SelectFromWhere (col_nm,id,(col,e_valu))}
	;

col_nm_def:
	| d=ID {[d]}
	| a=col_nm_def COMMA d=ID {a@[d]}
	;

col_def:
	| def=col {[def]}
	| others=col_def COMMA def=col {others@[def]}
	;

col:
	| name=ID col_data=col_type {(name, col_data)}
	;

col_type:
	| INT {"INT"}
	| STRING {"STRING"}
	| FLOAT {"FLOAT"}
	| BOOL {"BOOL"}
	| DATE {"DATE"}
	// | VARCHAR LPAREN len=INT_ACT RPAREN {"VARCHAR("^ string_of_int len^")"}
	;

row_vals:
	| d=data {[d]}
	| a=row_vals COMMA d=data {a@[d]}
	;

data:
	| i=INT_ACT {string_of_int i}
	| d=DATE_ACT {d}
	| f=FLOAT_ACT {string_of_float f}
	| a=ID {a}
	;