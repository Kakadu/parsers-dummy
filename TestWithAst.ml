open Helpers
open Printf


type options = {
  mutable filename    : string;
  mutable with_yacc   : bool;
  mutable with_ostap  : bool;
  mutable with_recdesc: bool;
  mutable with_comb   : bool;
}

let options = { filename="expr2.e"; with_yacc=false; with_ostap=false; with_recdesc=false; with_comb=false }

let yacc_output_file  =     "yacc.expr.out"
let ostap_output_file  =    "ostap.expr.out"

let () =
  Arg.parse [ ("-f",       Arg.String (fun s -> options.filename <- s), "input file name")
            ; ("-yacc",    Arg.Unit   (fun ()-> options.with_yacc<- true), "exectute yacc/menhir parsing")
            ; ("-comb",    Arg.Unit   (fun () -> options.with_comb       <- true), "execute combintor parsing")
            ; ("-recdesc", Arg.Unit   (fun () -> options.with_recdesc <- true), "execute resdesc parsing")
            ; ("-ostap",   Arg.Unit   (fun () -> options.with_ostap     <- true), "execute ostap parsing")
            ]
    (fun _ -> failwith "Anonymous arguments are not supported")
    "usage msg"

let () = printf "Using input file '%s'\n%!" options.filename

let clear_caches () =
  print_endline "clearing caches";
  Sys.command "sync && sudo bash -c \"echo 3 > /proc/sys/vm/drop_caches\"" |! ignore

(** Executing YACC printing *)
let run_lr () = if options.with_yacc then begin
  clear_caches ();
  print_endline "\n============================= YACC parsing and printing ...\n";
  clear_caches ();
  print_endline ("using output file " ^ yacc_output_file);
  let ch = open_in options.filename in

  let () =
    try
      let xs = (fun () ->
        let lexbuf = Lexing.from_channel ch in
        ExprYaccAst.program LexerExprAst.token lexbuf) |! eval_time "YACC  parsing"
      in
      let ps = (fun () -> xs |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
                |! eval_time "ast -> printer" in
      with_file yacc_output_file
        (fun ch -> ps |! List.iter (fprintf ch "%s\n"))
    with End_of_file -> print_endline "Some error"
  in
  close_in ch
end


let comb_output_file = "comb.expr.out"
open HackParserLiftingAst
let run_comb () = if options.with_comb then begin
    clear_caches ();
    print_endline "\n============================= Combinator parsing and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n"  comb_output_file;

    let do_parse, lexer_time = program source  in
    let (ans, parser_time)   = eval_time_2 do_parse in 
    Gc.compact (); 
    match ans with
      | Comb.Parsed(xs,_) ->
        printf "action 'combinator parsing' tooks %f time (%f lexer + %f parsing)\n%!"
          (lexer_time +. parser_time) lexer_time parser_time;
        let ps = (fun () -> xs |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
                |! eval_time "ast -> printer" in
        with_file comb_output_file  (fun ch -> ps |! List.iter (fprintf ch "%s\n"));
        flush stdout
      | Comb.Failed ->
        printf "TT\n";
        exit 1
    end

let recdesc_output_file = "recdesc.expr.out"
open RecDescAst
let run_recdesc () = if options.with_recdesc then begin
    clear_caches ();
    print_endline "\n============================= Recursive Descent and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n"  recdesc_output_file;

    let do_parse, lexer_time = program source  in
    let (ans, parser_time)    = eval_time_2 do_parse in
    printf "action hack parsing tooks %f time (%f lexer + %f parsing)\n%!"
      (lexer_time +. parser_time) lexer_time parser_time;
    let ps = (fun () -> ans |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString)) |! eval_time "ast->printer" in
    with_file recdesc_output_file (fun ch -> ps |! List.iter (fprintf ch "%s\n"));

    flush stdout
end
(*
let run_ostap () = if options.with_ostap then begin
    clear_caches ();

    print_endline "\n============================= Ostap and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n%!"  ostap_output_file;

    let (ans, parser_time)    = eval_time_2 (fun () -> ExprOstapAst.program (new OstapLexerExpr.t source)) in
    match ans with
      | Parsed((xs,_), _) ->
        printf "action 'ostap parsing' tooks %f time \n%!" parser_time;
        with_file ostap_output_file
          (fun ch -> xs |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString) |! List.iter (fprintf ch "%s\n"));
        flush stdout
      | Failed r ->
        printf "TT\n";
        Ostap.Reason.toString `All `Desc r |! print_endline;
        exit 1
    end
*)
let () =
  run_lr ();
  run_comb ();
  run_recdesc () (*;
  run_ostap () *)


