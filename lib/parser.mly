%token <Table> CREATE
%token EOF

%start <Ast.expr> prog

%% 

prog: 
    | e = expr; EOF { e }; 

expr:
    | t = TABLE { table t }