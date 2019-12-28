{
  open A1
  open A3
  exception Not_implemented
  exception Eof
  exception InvalidToken_raises
  exception Stringisempty
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
*)

let whitespace = [' ' '\t' '\n']+                  
let integer =  ['0'-'9']+                             
let digit =  ['0'-'9']                        
let lower_case = ['a'-'z']                         
let upper_case = ['A'-'Z']     
let symbals = [ '_' '''] 
let letter = (upper_case | lower_case | symbals)*  
let id =    upper_case (letter|digit)*

rule read = parse 
whitespace               { read lexbuf }
| integer as n           {INT(int_of_string n)}

| "->"                          {IMPLIES}
| ":"                           {COLON}
| "rec"                         {REC}

|  "T"                   {BOOL(true) }          
|  "F"                   {BOOL(false) }

|  "+"                   {PLUS }
|  "*"                   {TIMES }

|  "/\\"                 {CONJ }
|  "\\/"                 {DISJ }

|"("                      {LP}
|")"                      {RP}
|  "cmp"                 {CMP}

|  "if"                  {IF }
|  "then"                {THEN }
|  "else"                {ELSE }
|  "fi"                  {FI}


| "let"                         {LET}
| "in"                          {IN}
| "end"                         {END}
| "\\"                          {BACKSLASH}
| "."                           {DOT}
| "def"                         {DEF}
| ";"                           {SEMICOLON}
| "||"                          {PARALLEL}
| "local"                       {LOCAL}

|  id as s               {ID (s)}
|  eof                   {EOF}
| _                      { raise InvalidToken_raises }
