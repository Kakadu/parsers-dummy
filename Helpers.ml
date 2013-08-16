external (|!): 'a -> ('a -> 'b) -> 'b = "%revapply"

module Ref = struct
  let replace x ~f = x:= f !x
  let make x = ref x
end

open Ostap.Pretty

let listByNewline      = listBy (string "\n")
let listByNewlineBreak = listBy (seq [string "\n"; break])
(*
let is_keyword =
  let module S = Set.Make (String) in
  let s =
    List.fold_right
      S.add
      ["IF"   ; "THEN"; "ELSIF"; "ELSE"  ; "END" ; "MODULE"   ; "VAR"    ; "CONST"  ; "TYPE";
       "WHILE"; "DO"  ; "MOD"  ; "DIV"   ; "OR"  ; "PROCEDURE"; "CASE"   ; "OF"     ; "FOR" ;
       "TO"   ; "BY"  ; "ARRAY"; "RECORD"; "BEGIN"
      ]
      S.empty
  in
  fun name -> S.mem (Ostap.Matcher.Token.repr name) s
*)
module List = struct
  include List
  let mem_index_exn el xs =
    let rec helper n = function
      | h::tl when h=el -> n
      | _::tl  -> helper (n+1) tl
      | []     -> raise Not_found
    in
    helper 0 xs

  let mem_index el xs =
    try Some (mem_index_exn el xs)
    with Not_found -> None

end

module StringMap = struct
  include Map.Make(String)
  let add_counter k t =
    try let v = find k t in
        add k (v+1) t
    with Not_found  -> add k 1 t
end

let with_file name f =
  let ch = open_out name in
  let ans = f ch in
  let () = close_out ch in
  ans

let eval_time msg f =
  let timer = Timer.make () in
  let ans = f () in
  let d = timer () in
  Printf.printf "Action '%s' tooks %f processor time\n%!" msg d;
  ans

let eval_time_2 f =
  let timer = Timer.make () in
  let ans = f () in
  let d = timer () in
  ans,d

module Option = struct
    let map ~f = function None -> None | Some x -> Some (f x)
end

let read name =
  let inch = open_in_bin name in
  let len  = in_channel_length inch in
  let buf  = String.make len ' ' in
  really_input inch buf 0 len;
  close_in inch;
  buf

