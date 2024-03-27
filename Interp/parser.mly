%token CREATE
%token TABLE
%token DATABASE
%token <string> name
%token EOF

%start <Ast.expr> prog

%% 

prog: 
    | e = expr; EOF { e }; 

expr:
    | CREATE; TABLE; name {Command (CREATE, TABLE, name)}