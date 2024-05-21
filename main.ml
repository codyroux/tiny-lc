type term =
  | Var of int
  | Num of int
  | App of term * term
  | Lam of term
  | Plus of term * term
  | Ite of term * term * term

type input = { mutable pos : int; mutable str : string }

let glob = { pos = 0; str = "" }

let set_glob str =
  glob.pos <- 0;
  glob.str <- str

let peek () =
  String.get glob.str glob.pos

let eof () = glob.pos >= String.length glob.str

let pop () =
  if not (eof ()) then
    let head = peek() in
    glob.pos <- glob.pos + 1;
    head
  else failwith "Unexpected end of input"

let is_digit c =
  '0' <= c && c <= '9'

let to_digit () =
  let c = pop () in
  Char.code c - Char.code '0'

let space () =
  let d = pop () in
  if d == ' ' then
    ()
  else
  failwith ("Unexpected char "
            ^ Char.escaped d
            ^ " at position:"
            ^ Int.to_string glob.pos
            ^ " expected a space")

let parse_int () =
  let value = ref 0 in
  while not (eof ()) && is_digit (peek ()) do
    value := !value * 10;
    value := !value + to_digit ()
  done;
  !value

let parse_num () =
  let i = parse_int () in
  Num i

let parse_neg () =
  let _ = pop () in
  let i = parse_int () in
  Num (-i)

let parse_var () =
  let _ = pop () in
  let i = parse_int () in
  Var i

let rec parse_lam () =
  let _ = pop () in
  space ();
  Lam(parse_term ())
and parse_app () =
  let _ = pop () in
  space ();
  let t1 = parse_term () in
  space ();
  let t2 = parse_term () in
  App(t1, t2)
and  parse_term () =
  let c = peek () in
  if c == '$' then
    parse_var ()
  else if c == '@' then
    parse_app ()
  else if c == '\\' then
    parse_lam ()
  else if c == '?' then
    parse_ite ()
  else if c == '+' then
    parse_plus ()
  else if c == '-' then
    parse_neg ()
  else if is_digit c then
    parse_num ()
  else
  failwith ("Unexpected char "
            ^ Char.escaped c
            ^ " at position:"
            ^ Int.to_string glob.pos)

and parse_plus () =
  let _ = pop () in
  space ();
  let t1 = parse_term () in
  space ();
  let t2 = parse_term () in
  Plus(t1, t2)

and parse_ite () =
  let _ = pop () in
  space ();
  let t1 = parse_term () in
  space ();
  let t2 = parse_term () in
  space ();
  let t3 = parse_term () in
  Ite(t1, t2, t3)


let rec pretty_term t =
  match t with
  | Plus(t1, t2) -> "+ " ^ pretty_term t1 ^ " " ^ pretty_term t2
  | Ite(t1, t2, t3) ->
     "? " ^ pretty_term t1 ^ " " ^ pretty_term t2 ^ " " ^ pretty_term t3
  | Var i -> "$" ^ Int.to_string i
  | Num i -> Int.to_string i
  | App (t, u) -> "@ " ^ pretty_term t ^ " " ^ pretty_term u
  | Lam t -> "\\ " ^ pretty_term t


type value =
  | VNum of int
  | Clos of term * value list

let rec pretty_val v =
  match v with
  | VNum i -> Int.to_string i
  | Clos (t, vs) ->
     let vs = List.fold_left (fun s v -> s ^ ", " ^ pretty_val v) "" vs in
     "\\" ^ pretty_term t ^ "[" ^ vs ^ "]"

exception TypeMismatch of string

let rec eval t env =
  match t with
  | Var i -> List.nth env i
  | Num i -> VNum i
  | Plus(t1, t2) ->
     let v1 = eval t1 env in
     let v2 = eval t2 env in
     begin
       match v1, v2 with
       | VNum i, VNum j -> VNum (i + j)
       | _ ->
          raise
            (TypeMismatch
               ("Expected int * int, got "
                ^ pretty_val v1
                ^ " "
                ^ pretty_val v2))
     end
  | Ite(t1, t2, t3) ->
     let vcond = eval t1 env in
     begin
       match vcond with
       | VNum i ->
          if i <> 0 then
            eval t2 env
          else
            eval t3 env
       | _ ->
          raise
            (TypeMismatch
               ("Expected int, got "
                ^ pretty_val vcond))
     end
  | Lam t -> Clos (t, env)
  | App (t, u) ->
     let lval = eval t env in
     match lval with
     | Clos (t, env') ->
        let v = eval u env in
        eval t (v :: env')
     | _ ->
        raise
          (TypeMismatch
             ("Expected a lambda, got "
              ^ pretty_val lval
              ^ " "
              ^ pretty_term u))

let run t =
  eval t []

(* The Z combinator, which is a call by value fixed point for fix.*)
(* It's defined as:

    0    0  1   0  1 1 0     0  1   0  1 1 0
   \f. (\x. f (\v. x x v)) (\x. f (\v. x x v))
 *)
let z =
  let xxv = App (App (Var 1, Var 1), Var 0) in
  let lxxv = Lam xxv in
  let flxxv = App(Var 1, lxxv) in
  let lflxxv = Lam flxxv in
  let self_app = App(lflxxv, lflxxv) in
  Lam self_app

let decr t = Plus(t, Num (-1))

(* No recursion here so we have to lambda and then uze the Z combinator. *)
(*
    0  0 0     1           0     2   1      0
  \mul n m. if n != 0 then m + (mul (n - 1) m) else 0
*)
let mul =
  let rec_call = App(App(Var 2, decr (Var 1)), Var 0) in
  let rec_call_plus = Plus(Var 0, rec_call) in
  let ite = Ite(Var 1, rec_call_plus, Num 0) in
  Lam (Lam (Lam ite))

let fst = Lam (Lam (Var 1))

(* Run with: *)
(* ocamlbuild main.native && ./main.native *)
let () =
  let three_times_four = App(App(App(z, mul), Num 1000), Num 1000) in
  let decr_three = decr (Num 3) in
  let test = App(App(App(mul, fst), Num 1), Num 7) in
  set_glob "@ \\ + 3 $0 -2";
  set_glob "? 0 2 5";
  set_glob (pretty_term three_times_four);
  let t = parse_term () in
  print_endline ("parsed: " ^ pretty_term t);
  print_endline ("evaled: " ^ pretty_val (run t))
