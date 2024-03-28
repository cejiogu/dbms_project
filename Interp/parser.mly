// %token CREATE
// %token TABLE
// // %token DATABASE
// %token <string> NAME
// %token EOF

// %start <Ast.expr> prog

// %% 

// prog: 
//     | e = expr; EOF { e }; 

// expr:
//     | CREATE; TABLE; s = NAME {Command (CREATE, TABLE, s)}