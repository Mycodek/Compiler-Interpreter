%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID IDT
%token PLUS TIMES CONJ DISJ CMP IF THEN ELSE FI LP RP COLON IMPLIES REC
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF
%start exp_parser
/* %type <A1.definition> def_parser  */
%type <A1.exptree> exp_parser /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */
exp_parser:
  exp EOF   {$1}
;
exp:
    exp DISJ disj_exp                       {Disjunction($1,$3)}
    |disj_exp                               {$1}
;
disj_exp:
    disj_exp CONJ conj_exp                  {Conjunction($1,$3)}
    |conj_exp                               {$1}
;    
conj_exp:
    /* NOT conj_exp                             {Not($2)} */
    comp_exp                               {$1}
;
comp_exp:
    /* comp_exp EQ comp_exp                              {Equals($1,$3)} */
    /* |comp_exp GT comp_exp                             {GreaterT($1,$3)} */
    /* |comp_exp GT EQ comp_exp                          {GreaterTE($1,$4)} */
    /* |comp_exp LT EQ comp_exp                          {LessTE($1,$4)} */
    /* |comp_exp LT comp_exp                             {LessT($1,$3)}   */
    CMP comp_exp                            {Cmp($1)}    
    |add_sub_exp                            {$1}
;
add_sub_exp:
    add_sub_exp PLUS mult_div_mod_exp         { Add($1,$3) }
    /* |add_sub_exp MINUS mult_div_mod_exp       { Sub($1,$3) }  */
    |mult_div_mod_exp                         { $1 }
;
mult_div_mod_exp:
    mult_div_mod_exp TIMES abs_exp         {Mult($1,$3)}
    /* |mult_div_mod_exp DIV abs_exp          { Div($1,$3) } */
    /* |mult_div_mod_exp REM abs_exp               { Rem($1,$3) } */
    |abs_exp                               { $1 }
;
abs_exp:
    /* ABS neg_exp                                  {Abs($2)} */
    neg_exp                                        {$1}
;
neg_exp:
    /* TILDA iftexp                          {Negative($2)} */
    iftexp                                 {$1}
;
iftexp:
    IF exp THEN exp ELSE exp FI             {If_Then_Else($2,$4,$6)}    
    /* |PROJ LP INT COMMA INT RP exp           {Project(($3,$5),$7)}     */
    /* |LP tuple_case RP                       {Tuple((List.length $2), $2)} */
    |par_exp                                {$1}
;
par_exp:
    LP exp RP                           {InParen($2)}
    |def_exp                            { $1 }
;
def_exp:
    /* LET defination IN exp END               {Let($2,$4)} */
    |BACKSLASH ID DOT exp                   {FunctioinAbsrationWT($2,$4)}
    |BACKSLASH colexp DOT exp               {FunctionAbstraction($2,$4)}
    |exp LP exp RP                      {FunctionCall($1,$3)}
    |constant                           {$1}
;
constant:
    ID                                  { Var($1) }
    |BOOL                               { B($1) }
    |INT                                { N($1) }
;
/* tuple_case: */
    /* exp COMMA tuple_case                { $1::$3 } */
    /* |exp COMMA exp                      { [$1;$3] } */
/* ; */

/* colexp: */
    /* ID COLON deftype                     {Colon($1,$3)} */

/* deftype: */
    /* deftype IMPLIES deftype             {Tfunc($1,$3)} */
    /* |tuptype                            {$1} */

/* tuptype: */
    /* LP tupexp_typ RP                      {Ttuple($2)} */
    /* |stype                                {$1} */

/* stype: */
    /* ID                                              {if($1 = "Tint") then Tint else if($1 = "Tbool") then Tbool else Tunit ; }  */

/* tupexp_typ: */
    /* deftype TIMES tupexp_typ                        { $1::$3 } */
    /* |deftype TIMES deftype                          { [$1;$3] } */
/* ;      */

/* Implement the grammar rules for definitions, which may use the parser for expression  */

/* def_parser: */
    /* defination EOF                                        { $1 } */
/* ; */

/* defination: */
  /* defination SEMICOLON ldef                               { match $1 with  */
                                                      /* Sequence(deflist) -> Sequence(deflist@[$3]) */
                                                     /* | _ -> Sequence( [$1]@[$3] ) } */
  /* |defination PARALLEL ldef                                { match $1 with  */
                                                      /* Parallel(deflist) -> Parallel(deflist@[$3]) */
                                                      /* | _ -> Parallel( [$1]@[$3] )  } */
  /* |ldef                                             { $1 }      */
/* ; */

/* ldef: */
  /* LOCAL defination IN defination END                             { Local($2,$4) }       */
  /* |DEF colexp EQ exp                                             { Simple($2,$4) } */
  /* |REC DEF colexp EQ exp                                         { SimpleRec($3,$5) } */
  /* |DEF ID EQ exp                                                 { SimpleWT($2,$4) } */
/* ; */








/* 
colexp:
    ID COLON deftype                     {Colon($1,$3)}

deftype:
    deftype IMPLIES deftype             {Typefunc($1,$3)}
    |tuptype                            {$1}

tuptype:
    LP tupexp_typ RP                      {Tuples($2)}
    |stype                                {$1}

stype:
    ID                                     {S($1)} 

tupexp_typ:
    deftype TIMES tupexp_typ                        { $1::$3 }
    |deftype TIMES deftype                          { [$1;$3] }
;  */