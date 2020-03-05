(* parser-specification file *)

(*
  BEGIN first section - declarations

  - token and type specifications, precedence directives and other output directives
*)
%token SKIP
%token DEFEQ
%token WHILE
%token IF ELSE
%token RETURN
%token FUNCTION
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA SEMICOLON
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> STRING
%token PLUS MINUS TIMES DIVIDE GT LT EGT ELT
%token EOF

%left GT LT EGT ELT
%left PLUS MINUS
%left TIMES DIVIDE
%left SEMICOLON

%nonassoc binopt_prec
%nonassoc unopt_prec

%type <Expr.t> prog_expr_target
%type <Stmt.t> prog_stmt_target
%type <Prog.t> prog_target

%start prog_target prog_expr_target prog_stmt_target
%% (* separator line *)
(* END first section - declarations *)

(*
  BEGIN - second section - grammar and rules

  - specifying the grammar of the language to be parsed, specifying the productions
  - productions are organized into rules, where each rule lists all
    the possible productions for a given nonterminal.
*)

prog_expr_target:
  | e = expr_target; EOF; { e }

prog_stmt_target:
  | s = stmt_target; EOF; { s }

prog_target:
  | funcs = separated_list (SEMICOLON, proc_target); EOF;
   { Prog.create_prog funcs }

proc_target:
  | FUNCTION; f = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; LBRACE; s = stmt_target; RBRACE
   { Func.create_func f vars s }

(*
  The pipes separate the individual productions, and the curly braces contain a semantic action:
    OCaml code that generates the OCaml value corresponding to the production in question.

  Semantic actions are arbitrary OCaml expressions that are evaluated during parsing
    to produce values that are attached to the nonterminal in the rule.
*)

(* v ::= f | i | b | s *)
val_target:
  | f = FLOAT;
    { Val.Flt f }
  | i = INT;
    { Val.Int i }
  | b = BOOLEAN;
    { Val.Bool b}
  | s = STRING;
    { Val.Str s }

(* e ::= v | x | -e | e+e | f(e) | (e) *)
expr_target:
  | v = val_target;
    { Expr.Val v }
  | v = VAR;
    { Expr.Var v }
  | MINUS; e = expr_target;
    { Expr.UnOpt (Expr.Neg, e) } %prec unopt_prec
  | e1 = expr_target; bop = op_target; e2 = expr_target;
    { Expr.BinOpt (bop, e1, e2) } %prec binopt_prec
  | v = VAR; LPAREN; es = separated_list (COMMA, expr_target); RPAREN;
    { Expr.Call (v, es) }
  | LPAREN; e = expr_target; RPAREN;
    { e }

(* s ::= skip | x := e | s1, s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e *)
stmt_target:
  | SKIP;
    { Stmt.Skip }
  | v = VAR; DEFEQ; e = expr_target;
    { Stmt.Assign (v, e) }
  | s1 = stmt_target; SEMICOLON; s2 = stmt_target;
    { Stmt.Seq (s1, s2) }
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s1 = stmt_target; RBRACE; ELSE; LBRACE; s2 = stmt_target; RBRACE;
    { Stmt.If (e, s1, s2)}
  | WHILE; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_target; RBRACE;
    { Stmt.While (e, s) }
  | RETURN; e = expr_target;
    { Stmt.Return e }

op_target:
  | MINUS  { Expr.Minus }
  | PLUS   { Expr.Plus }
  | TIMES  { Expr.Times }
  | DIVIDE { Expr.Div }
  | GT     { Expr.Gt }
  | LT     { Expr.Lt }
  | EGT    { Expr.Egt }
  | ELT    { Expr.Elt }
