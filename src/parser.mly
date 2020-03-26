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
%token UNDEFINED
%token FUNCTION
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token PERIOD COMMA SEMICOLON COLON
%token DELETE
%token <float> FLOAT
%token <int> INT
%token <bool> BOOLEAN
%token <string> VAR
%token <string> STRING
%token LOG_AND LOG_OR
%token PLUS MINUS TIMES DIVIDE EQUAL GT LT EGT ELT IN NOT
%token EOF

%left IN LOG_AND LOG_OR
%left GT LT EGT ELT
%left PLUS MINUS
%left TIMES DIVIDE
%left EQUAL
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
   { Prog.create funcs }

proc_target:
  | FUNCTION; f = VAR; LPAREN; vars = separated_list (COMMA, VAR); RPAREN; LBRACE; s = stmt_target; RBRACE
   { Func.create f vars s }

(*
  The pipes separate the individual productions, and the curly braces contain a semantic action:
    OCaml code that generates the OCaml value corresponding to the production in question.

  Semantic actions are arbitrary OCaml expressions that are evaluated during parsing
    to produce values that are attached to the nonterminal in the rule.
*)

(* v ::= f | i | b | s *)
val_target:
  | UNDEFINED;
    { Val.Undef }
  | f = FLOAT;
    { Val.Flt f }
  | i = INT;
    { Val.Int i }
  | b = BOOLEAN;
    { Val.Bool b }
  | s = STRING;
    { let len = String.length s in
      let sub = String.sub s 1 (len - 2) in
      Val.Str sub } (* Remove the double-quote characters from the parsed string *)

(* e ::= {} | {f:e} | e.f | e[f] | v | x | -e | e+e | f(e) | (e) *)
expr_target:
  | LBRACE; fes = separated_list (COMMA, fv_target); RBRACE;
    { Expr.NewObj (fes) }
  | e = expr_target; PERIOD; f = VAR;
    { Expr.Access (e, Expr.Val (Str f)) }
  | e = expr_target; LBRACK; f = expr_target; RBRACK;
    { Expr.Access (e, f) }
  | v = val_target;
    { Expr.Val v }
  | v = VAR;
    { Expr.Var v }
  | MINUS; e = expr_target;
    { Expr.UnOpt (Expr.Neg, e) } %prec unopt_prec
  | NOT; e = expr_target;
    { Expr.UnOpt (Expr.Not, e) } %prec unopt_prec
  | e1 = expr_target; bop = op_target; e2 = expr_target;
    { Expr.BinOpt (bop, e1, e2) } %prec binopt_prec
  | f = expr_target; LPAREN; es = separated_list (COMMA, expr_target); RPAREN;
    { Expr.Call (f, es) }
  | LPAREN; e = expr_target; RPAREN;
    { e }

fv_target:
  | f = VAR; COLON; e = expr_target;
    { (f, e) }

(* s ::= e.f := e | delete e.f | skip | x := e | s1, s2 | if (e) { s1 } else { s2 } | while (e) { s } | return e *)
stmt_target:
  | e1 = VAR; PERIOD; f = VAR; DEFEQ; e2 = expr_target;
    { Stmt.FieldAssign (Expr.Var e1, f, e2) }
  | DELETE; e = VAR; f = VAR;
    { Stmt.FieldDelete (Expr.Var e, f) }
  | SKIP;
    { Stmt.Skip }
  | v = VAR; DEFEQ; e = expr_target;
    { Stmt.Assign (v, e) }
  | s1 = stmt_target; SEMICOLON; s2 = stmt_target;
    { Stmt.Seq (s1, s2) }
  | exps_stmts = list (ifelse_target);
    { Stmt.If (exps_stmts) }
  | WHILE; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_target; RBRACE;
    { Stmt.While (e, s) }
  | RETURN; e = expr_target;
    { Stmt.Return e }

(* if (e) { s } | else if (e) { s } | else { s } *)
ifelse_target:
  | if_t = if_target;
    { if_t }
  | ELSE; if_t = if_target;
    { if_t }
  | ELSE; LBRACE; s = stmt_target; RBRACE;
    { (None, s) }

if_target:
  | IF; LPAREN; e = expr_target; RPAREN; LBRACE; s = stmt_target; RBRACE;
    { (Some e, s) }

op_target:
  | MINUS   { Expr.Minus }
  | PLUS    { Expr.Plus }
  | TIMES   { Expr.Times }
  | DIVIDE  { Expr.Div }
  | EQUAL   { Expr.Equal }
  | GT      { Expr.Gt }
  | LT      { Expr.Lt }
  | EGT     { Expr.Egt }
  | ELT     { Expr.Elt }
  | LOG_AND { Expr.Log_And }
  | LOG_OR  { Expr.Log_Or }
  | IN      { Expr.InObj }
