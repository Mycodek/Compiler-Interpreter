(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception Exception

type expr =
    V of string 
    | Lambda of expr * expr
    | App of expr * expr
    | Plus of expr * expr
    | Mult of expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Paran of expr
    | Bool of bool 
    | Integer of int 
    | Cmp of expr 
    | If_Then_Else of expr * expr * expr

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Tfunc of (exptype * exptype)

(* The type of value returned by the definitional interpreter. *)
(* type value = NumVal of int | BoolVal of bool *)

(* The language should contain the following types of expressions:  integers and booleans *)
type closure = Cl of (expr * ((string * closure) list))

(* type ValueClosure = vcl of value * ((string * closure) list) *)


(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | N of bigint | B of bool| CMP | PLUS | MULT | CONJ | DISJ | PARAN | IFTE | RET | APP | CLOS of string * opcode list

type answer = NumAns of bigint | BoolAns of bool| Cla of (string * opcode list * (string * answer) list)  

let rec searchInTable str t = match t with
    [] -> raise Exception
    |x::xs -> match x with (name,cls) -> if(str = name) then cls else searchInTable str xs;;  

let rec krivineMc cls stk = match cls with Cl(e,table) -> match e with
    Bool(b)                     ->  BoolAns(b)
    | Integer(n)                ->  NumAns(mk_big n)
    | V(str)                    ->  krivineMc (searchInTable str table) stk
    | Paran(e)                  ->  krivineMc (Cl(e,table)) stk
    | Cmp(e)                    ->  let r = match krivineMc (Cl(e,table) )stk with NumAns(n) -> if (gt n (mk_big 0)) then BoolAns(true) else BoolAns(false) in r;
    | Plus (e1,e2)              ->  let ad = match (krivineMc (Cl(e1,table)) stk,krivineMc (Cl(e2,table)) stk) with (NumAns(n1),NumAns(n2)) -> NumAns(add n1 n2)
                                                                                                        | _ -> raise Exception in ad;
    | Mult (e1,e2)              ->  let ad = match (krivineMc (Cl(e1,table)) stk,krivineMc (Cl(e2,table)) stk) with (NumAns(n1),NumAns(n2)) -> NumAns(mult n1 n2)
                                                                                                        | _ -> raise Exception in ad;
    | And (e1,e2)               ->  let ad = match (krivineMc (Cl(e1,table)) stk,krivineMc (Cl(e2,table)) stk) with (BoolAns(b1),BoolAns(b2)) -> BoolAns(b1 && b2)
                                                                                                        | _ -> raise Exception in ad;
    | Or (e1,e2)                ->  let ad = match (krivineMc (Cl(e1,table)) stk,krivineMc (Cl(e2,table)) stk) with (BoolAns(b1),BoolAns(b2)) -> BoolAns(b1 || b2)
                                                                                                        | _ -> raise Exception in ad;
    | If_Then_Else(e1,e2,e3)    ->  let ad = match (krivineMc (Cl(e1,table)) stk) with BoolAns(b) -> if b then (krivineMc (Cl(e2,table)) stk) else (krivineMc (Cl(e3,table)) stk) in ad;
    | Lambda (e1,e2)            ->  let ad = match e1 with V(str) -> let r = match stk with s::st -> (krivineMc (Cl(e2,([(str,s)]@table))) st) in r;
                                                                                    | _ -> raise Exception in ad;
    | App (e1,e2)               ->  krivineMc (Cl(e1,table)) ([Cl(e2,table)]@stk)           


    (* type opcode = VAR of string | NCONST of bigint | BCONST of bool| PLUS | MULT | CONJ | DISJ | PAREN | IFTE | RET | APP *)

let rec combine l = match l with
    [] -> []
    | x::xs -> x @ combine xs
let rec compile ex =  match ex with
    Bool(b)                     ->  [B(b)]
    | Integer(n)                ->  [N(mk_big n)]
    | V(str)                    ->  [VAR(str)]
    | Paran(e)                  ->  (compile e) @  [PARAN]
    | Cmp(e)                    ->  (compile e) @  [CMP] 
    | Plus (t1,t2)              ->  (compile t1) @ (compile t2) @ [PLUS] 
    | Mult (t1,t2)              ->  (compile t1) @ (compile t2) @ [MULT]
    | And (t1,t2)               ->  (compile t1) @ (compile t2) @ [CONJ]
    | Or (t1,t2)                ->  (compile t1) @ (compile t2) @ [DISJ]
    | If_Then_Else(t1,t2,t3)    ->  (compile t1) @ (compile t2) @ (compile t3)  @ [IFTE]
    | Lambda (e1,e2)            ->  let a = match e1 with V(x) -> [CLOS(x , ((compile e2)@[RET]) )]
                                                        | _ -> raise Exception in a; 
    | App (e1,e2)               ->  (compile e1) @ (compile e2) @ [APP] 
    (* val secdMc: (answer list) -> (string * answer) list -> (opcode list) -> ((answer list) * (string * answer)list * (opcode list))  -> answer *)

    (* val secdMc: ans_Stack -> table -> opcode list -> dump D  => answer*)

(* this function converts answers to bigint as they're needed in comp which is nthing but calculation part of stack machine *)
let a2Bint t = match t with
    NumAns (n) -> n
(* this function converts answers to bool as they're needed in comp which is nthing but calculation part of stack machine *)
let a2bool t = match t with
    BoolAns (b) -> b    

let rec calci x y t = match t with 
    CMP               -> BoolAns (gt (a2Bint x) (mk_big 0))
    | PLUS            -> NumAns  (add  (a2Bint x) (a2Bint y)) 
    | MULT            -> NumAns  (mult (a2Bint x) (a2Bint y))
    | CONJ            -> BoolAns ((a2bool x) && (a2bool y))
    | DISJ            -> BoolAns ((a2bool x) || (a2bool y))
    ;;

let rec findV str tab = match tab with
    [] -> raise Exception
    |x::xs -> match x with (sd,ad) -> if(sd = str) then ad else findV str xs;;

let rec secdMc stack table oplist dump = match oplist with
    [] -> let a = match stack with s :: st -> s in a;        
    |x :: xs -> let gf = match x with
                VAR(str)    ->  secdMc ([(findV str table)]@stack) table xs dump ;
                | N(n)      ->  secdMc ([NumAns(n)]@stack) table xs dump ;
                | B(b)      ->  secdMc ([BoolAns(b)]@stack) table xs dump ;
                | CMP       ->  let ad = match stack with s::st -> (secdMc ([calci s s x]@st) table xs dump) in ad;
                | PLUS      ->  let ad = match stack with s::st::sst -> (secdMc ([calci s st x]@sst) table xs dump) in ad;
                | MULT      ->  let ad = match stack with s::st::sst -> (secdMc ([calci s st x]@sst) table xs dump) in ad;
                | CONJ      ->  let ad = match stack with s::st::sst -> (secdMc ([calci s st x]@sst) table xs dump) in ad;
                | DISJ      ->  let ad = match stack with s::st::sst -> (secdMc ([calci s st x]@sst) table xs dump) in ad;
                | PARAN     ->  secdMc stack table xs dump ;
                | IFTE      ->  let ad =  match stack with
                                            s::st::sst::rstk -> match sst with 
                                            BoolAns(b) -> if b then secdMc ([st]@stack) table xs dump
                                                        else secdMc ([s]@stack) table xs dump in ad;
                | CLOS(str,opl) ->  secdMc  ([Cla(str,opl,table)]@stack) table xs dump;  
                | APP       ->  let ry= match stack with s :: ss :: sst -> match ss with Cla(str,opl,tab) -> secdMc [] ([(str,s)]@tab) opl ([(sst,table,xs)]@dump) in ry;
                | RET       ->  let re= match stack with sr :: srt -> match dump with d :: ds -> match d with (st,tab,xr) -> secdMc ([sr]@st) tab xr ds; in re;
            in gf;;

(* let g =   ([("x",NumAns(mk_big 3));("r",NumAns(mk_big 5))]);; *)
(* let checkSECD ex = secdMc [] g (compile ex) [];;  *)
(* let checkKrivine ex = krivineMc (Cl(ex,g)) [];;            *)


(* check (If_Then_Else (Cmp (Integer (-1)),App(Lambda (V "x",Plus (Integer 3,V "x")),Integer 32),Integer (-2))) *)