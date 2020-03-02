open Printf
open Expr
open Stmt
open Val
open Interpreter

let print_store (sto : Store.t) : unit =
  print_endline "\nStore contents:\n--------------";
  print_endline (Store.to_string sto);
  print_endline "--------------\n"


let main_expr (prog : Prog.t) (sto : Store.t) : unit =
  (* 3.5 * 2 *)
  let e1 = BinOpt (Times, Val (Flt 3.5), Val (Int 2))
  (* 7 *)
  and e2 = UnOpt (Neg, Val (Int (-7)))
  (* x + 2 *)
  and ex = BinOpt (Plus, Var "x", Val (Int 2))
  (* y - 3 *)
  and ey = BinOpt (Minus, Var "y", Val (Flt 3.)) in
  print_endline "Expressions:\n---------------------";
  printf "e1 => %s = %s;\n" (Expr.str e1) (Val.str (eval_expr prog sto e1));
  printf "e2 => %s = %s;\n" (Expr.str e2) (Val.str (eval_expr prog sto e2));
  printf "ex => %s = %s;\n" (Expr.str ex) (Val.str (eval_expr prog sto ex));
  printf "ey => %s = %s;\n" (Expr.str ey) (Val.str (eval_expr prog sto ey));
  print_endline "---------------------\n"


let main_stmt (prog : Prog.t) (sto : Store.t) : unit =
  let s1 = Skip
  (* x = 3.5 *)
  and s2 = Assign ("x", Val (Flt 3.5))
  (* z = 1; if (z) { z = 2 } else { z = 0 } *)
  and s3 = Seq (Assign("z", Val (Str "variable z")), If(Val (Bool true), Assign("w", Val (Bool false)), Assign("w", Val (Int 0))))
  (* if (y) { x = 1 } *)
  and s4 = If (BinOpt (Gt, Var "x", Val (Flt 2.)), Assign ("x", Val (Flt 2.)), Skip)
  (* while (x) { if (y) { x = x - 1 } } *)
  and s5 = While (BinOpt (Egt, Var "x", Val (Flt 0.)), Assign ("x", BinOpt (Minus, Var "x", Val (Flt 1.)))) in
  print_endline "Statements:\n------------------";
  printf "s1 => %s\n" (Stmt.str s1);
  printf "s2 => %s\n" (Stmt.str s2); ignore (eval_stmt prog sto s2);
  printf "s3 => %s\n" (Stmt.str s3); ignore (eval_stmt prog sto s3);
  printf "s4 => %s\n" (Stmt.str s4); ignore (eval_stmt prog sto s4);
  printf "s5 => %s\n" (Stmt.str s5); ignore (eval_stmt prog sto s5);
  print_endline "------------------"


let main_factorial (prog : Prog.t) (sto : Store.t) : unit =
  let s1 = Assign ("y", Val (Flt 1.)) and
  s2 = Assign ("x", Val (Flt 4.)) and
  s3 = Assign ("y", BinOpt(Times, Var "y", Var "x")) and
  s4 = Assign ("x", BinOpt(Minus, Var "x", Val (Flt 1.))) in
  let s12 = Seq(s1, s2) and
    s34 = Seq (s3, s4) in
  let swhile = While(BinOpt(Gt, Var "x", Val (Flt 0.)), s34) in
  let s = Seq(s12, swhile) in
  ignore (eval_stmt prog sto s); print_endline "Factorial of 4 evaluated!"


let main_parse_files (prog : Prog.t) : unit =
  Parsing_utils.(
    let expr_contents = load_file "expr_test_file" and
    stmt_contents = load_file "stmt_test_file" in
    let e = parse_expr expr_contents and
      s = parse_stmt stmt_contents in
    Store.(
      let store = create_store [] in
      ignore (eval_stmt prog store s);
      print_endline "\nParsed expression file:\n---------------";
      print_endline (Expr.str e);
      print_endline "---------------";
      print_endline "\nParsed statement file:\n---------------";
      print_endline (Stmt.str s);
      print_endline "---------------";
      print_store store
    )
  )


;;
Store.(
  let local_store = create_store [("x", Flt 2.); ("y", Flt 3.)] and
  empty_store = create_store [] and
  main = Hashtbl.create 511 in

  print_store local_store;
  main_expr main local_store;
  main_stmt main local_store;
  print_store local_store;

  main_factorial main empty_store;
  print_store empty_store;

  main_parse_files main
)
