open A0

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


val compile: expr -> opcode list
(* val krivineMc: closure -> stack => answer *)
val krivineMc: closure -> closure list -> answer
(* val secdMc: ans_Stack -> table -> opcode list -> dump D  => answer*)
val secdMc: (answer list) -> (string * answer) list -> (opcode list) -> ((answer list) * (string * answer)list * (opcode list)) list  -> answer

(* val check: expr -> answer;; *)
