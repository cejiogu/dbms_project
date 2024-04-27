%{
open Ast
open Final_project
%}

%token <int> INT_ACT
%token <string> ID
%token CREATE
%token TABLE
%token SCHEMA
%token SELECT
%token FROM
%token ALTER
%token ADD
%token INT
%token STRING
%token BOOL
%token DATE
%token FLOAT
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

