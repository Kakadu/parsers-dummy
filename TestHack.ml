open Helpers
open Printf


type options = {
  mutable filename  : string;
  mutable with_yacc : bool;
  mutable with_ostap: bool;
  mutable with_recdesc  : bool;
  mutable with_nolift   : bool;
}
let ostap_output_file =     "ostap.expr.out"
let yacc_output_file  =     "yacc.expr.out"
let recdesc_output_file =   "recdesc.expr.out"
let nolift_output_file  =  "nolift.expr.out"

(*
let () =
  Gc.(
    let c = get () in
    c.minor_heap_size <- 536870912*2;
    c.max_overhead <-      1000000;
    set c
  )
  *)
let options = { filename="expr2.e"; with_yacc=false; with_ostap=false; with_recdesc=false; with_nolift=false }

let () =
  Arg.parse [ ("-f", Arg.String (fun s -> options.filename <- s), "input file name")
            ; ("-yacc", Arg.Unit(fun ()-> options.with_yacc<- true), "exectute yacc parsing")
            ; ("-comb", Arg.Unit (fun () -> options.with_ostap <- true), "execute combinator parsing")
            ; ("-nolift", Arg.Unit (fun () -> options.with_nolift <- true), "execute not lifted combinator parsing")
            ]
    (fun _ -> failwith "Anonymous arguments are not supported")
    "usage msg"

let () = printf "Using input file '%s'\n%!" options.filename

(** Executing YACC printing *)
let run_yacc () = if options.with_yacc then begin
  print_endline "\n============================= YACC parsing and printing ...\n";
  print_endline ("using output file " ^ yacc_output_file);
  let ch = open_in options.filename in

  let () =
    try
      let ans = (fun () ->
        let lexbuf = Lexing.from_channel ch in
        ExprYacc.program LexerExpr.token lexbuf) |! eval_time "YACC  parsing" in
      with_file yacc_output_file (fun ch -> ans |! List.iter (fun s -> s |! Ostap.Pretty.toString |! fprintf ch "%s\n"))
    with End_of_file -> print_endline "Some error"
  in
  close_in ch
end


let hack_output_file = "hack.expr.out"

open HackParserLifting

(** Executing combinator printing *)
let run_comb () = if options.with_ostap then begin
    print_endline "\n============================= Hack parsing and printing...";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n"  hack_output_file;

    let do_parse, lexer_time = program source  in
    let ans, parser_time    = eval_time_2 do_parse in
    Gc.compact ();
    match ans with
      | Comb.Parsed (xs,_) ->
        with_file hack_output_file
          (fun ch -> xs |! List.map Ostap.Pretty.toString |! List.iter (fprintf ch "%s\n"));
        printf "action hack parsing tooks %f time (%f lexer + %f parsing)\n%!"
          (lexer_time +. parser_time) lexer_time parser_time;
        flush stdout
      | Comb.Failed     ->
        printf "TT\n";
        exit 1
end

open HackParser

(** Executing not lifted combinator printing *)
let run_nolifted () = if options.with_nolift then begin
    print_endline "\n============================= Not lifted combinator parsing and printing...";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n"  hack_output_file;

    let do_parse, lexer_time = program source  in
    let ans, parser_time    = eval_time_2 do_parse in
    Gc.compact ();
    match ans with
      | Comb.Parsed (xs,_) ->
        with_file hack_output_file
          (fun ch -> xs |! List.map Ostap.Pretty.toString |! List.iter (fprintf ch "%s\n"));
        printf "action 'hack parsing nolifted' tooks %f time (%f lexer + %f parsing)\n%!"
          (lexer_time +. parser_time) lexer_time parser_time;
        flush stdout
      | Comb.Failed     ->
        printf "TT\n";
        exit 1
end

let clear_caches () =
  print_endline "clearing caches";
  Sys.command "sync && sudo bash -c \"echo 3 > /proc/sys/vm/drop_caches\"" |! ignore

let () =
  clear_caches ();
  run_yacc ();
  clear_caches ();
  run_nolifted ();
  clear_caches ();
  run_comb ()


