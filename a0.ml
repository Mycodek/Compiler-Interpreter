type bigint = sign * int list
and sign = Neg | NonNeg
exception Not_implemented
exception Division_By_Zero

(* THIS FUNCTION REC. FIND THE LENGTH OF ANY TYPE OF LIST *)
let rec length l = match l with
	 	[]->0
        |(s::ss)->1+(length ss)
        ;;
(*THIS FUNCTION REC. REVERSE WHOLE LIST OF ANY TYPE  *)
let rec rev l = match l with
	 	[] -> []
		 |(x :: xs) -> rev (xs) @ [x] ;;
(*THIS FUNCTION GIVE US THE SIGN EITHER ITS NEG OR NONNEG OF BIG INTEGER  *)
let signof x = match x with
		(s,_) -> s;;
(*THIS FUNCTION GIVE US THE LIST OF BIG INTEDER  *)
let listof x = match x with
		(_,l) -> l;;		
(*THIS FUNCTION GIVE US THE NEGATION OF GIVEN BIGINT  *)
let minus x = if x=(NonNeg,listof x) then (Neg,listof x) 
		else (NonNeg,listof x)
		;; 
(*THIS FUNCTION GIVE US THE ABSOLUTE VALUE OF GIVEN BIGINT  *)
let abs x = if  x=(Neg,listof x) then (NonNeg,listof x) 
			else x
			;;
(*THIS FUNCTION GIVE US A BOOLEAN VALUE WETHER THE LIST L1 IS GREATER THEN L2 OR NOT  *)
let rec greater l1 l2 = match (l1,l2) with
    ([],[]) -> false
    |([],_) -> false
    |(_,[]) -> true
    |(x::xs,y::ys) -> if (length l1) > (length l2) then true
									else ( 
											if (length l1) < (length l2) then false
											else (
													if x < y then false
													else (
															if x > y then true
															else greater xs ys
													)
											)
									)
		;;		
(*THIS FUNCTION GIVE US A BOOLEAN VALUE WETHER GIVEN TWO BIGINT ARE SAME OR NOT  *)		
let eq x y = (signof x = signof y)&&(listof x = listof y);;
(*THIS FUNCTION GIVE US A BOOLEAN VALUE WETHER BIGINT X GREATER THEN Y OR NOT  *)		
let gt x y = match (signof x,signof y) with
		(Neg,NonNeg) 		->  false
		|(NonNeg,Neg)		->  true
		|(NonNeg,NonNeg) ->  greater (listof x) (listof y) 
		|(Neg,Neg)			->  greater (listof y) (listof x)
		;;
(*THIS FUNCTION GIVE US A BOOLEAN VALUE WETHER BIGINT X GREATER THEN OR EQUAL TO Y OR NOT  *)		
let geq x y = (gt x y) || (eq x y);;
(*THIS FUNCTION GIVE US A BOOLEAN VALUE WETHER BIGINT X LESS THEN Y OR NOT  *)		
let lt x y = not (geq x y);;
(*THIS FUNCTION GIVE US A BOOLEAN VALUE WETHER BIGINT X LESS THEN OR EQUAL TO Y OR NOT  *)		
let leq x y = not (gt x y);;    
(* IT IS SUB PART OF FUNCTION USING FOR ADDE WHICH ADD TWO LIST FROM THEIR HEAD AND SEND CARRY TO ITS TAIL AND THEN USE REC.
FOR TAIL THATS HOW IT MOVES *)
let rec addcheck l1 l2 c = match (l1,l2) with
		([],[]) -> if c = 0 then [] else [c] 
		|([],y::ys) ->  if (y+c) > 9 then  addcheck ys [1] 0 @ [(y+c-10)] else rev ys @ [(y+c)]
		|(x::xs,[]) ->  if (x+c) > 9 then  addcheck xs [1] 0 @ [(x+c-10)] else rev xs @ [(x+c)]
		|(x::xs,y::ys) -> if (x+y+c) > 9 then  addcheck xs ys 1 @ [(x+y+c-10)]	else addcheck xs ys 0 @ [(x+y+c)]
		;;
(*THIS FUNTION JUST HERE BECUASE OF THAT IT REV AND SEND THE LISTS TO ADDCHECK  *)
let adde l1 l2 = match (l1,l2) with
		([],[]) -> [0]
		|([],_) -> l2
		|(_,[]) -> l1
		|(_,_)  ->  addcheck (rev l1) (rev l2) 0 
		;;
(*THIS FUNCTION HERE FOR REMOVEING ALL LEADING ZEROS WHICH OCOUR DUE TO SUBTRACTION  *)
let rec remZ l = match l with 
		[] -> []
		|x::xs -> if x=0 then remZ xs else l 
		;;
(*THIS FUNCTION CORRESPOMDING TO SUBTRACT TWO LIST STARTING FROM HEAD AND THEN REC. COMPUTE SAME FOR THIER TAIL  *)
let rec subsub l1 l2 c = match (l1,l2) with
		([],[]) -> []
		|([],_) -> []
		|(x::xs,[]) ->  if (x-c) < 0 then subsub xs [] 1 @ [x-c+10] 
										else(
												if (x-c)=0 then (
													if xs = [] then []
													else rev xs @ [0]
													)
												else rev xs @ [x-c]	 
												) 
		|(x::xs,y::ys) ->  if (x-y-c)<0 then subsub xs ys 1 @ [x+10-y-c] 
												else subsub xs ys 0 @ [x-y-c]
				;; 
(*THIS FUNCITON IS JUST REVERSEING GIVEN LISTS AND THEN SEND THEM TO SUBSUB AND ALSO ENSURE THAT THEY SHOULD NOT HAVE 
ANY LEADING ZEROS BY VALLING REMZ  *)
let subs l1 l2 c = remZ (subsub (rev l1) (rev l2)	c) ;;
(*THIS FUNCTION IS WHAT IT MEAN TO YOU WITH ENSURING THIER SIGN  *)
let add x y = match (signof x,signof y) with
	(Neg,NonNeg) 		-> if gt ( abs x ) ( abs y ) then ( Neg , ( subs ( listof x ) ( listof y) 0 ) ) else ( NonNeg , ( subs ( listof y ) ( listof x ) 0 ) ) 
	|( NonNeg,Neg )		-> if lt ( abs x ) ( abs y ) then ( Neg , ( subs ( listof y ) ( listof x ) 0 ) ) else ( NonNeg , ( subs ( listof x ) ( listof y ) 0 ) )
	|( NonNeg,NonNeg ) -> ( NonNeg , adde ( listof x ) ( listof y ) )
	|( Neg,Neg )			->  ( Neg , adde ( listof x ) ( listof y ) )
	;; 
       

(*THIS FUNCTION IS WHAT IT MEAN TO YOU WIHT MAINTAINING THEIR SIGN *)
let sub x y = match (signof x,signof y) with
		(Neg,NonNeg) 		-> (Neg,adde (listof x) (listof y))
		|(NonNeg,Neg)		-> (NonNeg,adde (listof x) (listof y))
		|(NonNeg,NonNeg) -> if gt x y then (NonNeg, (subs (listof x) (listof y) 0) ) else (Neg, (subs (listof y) (listof x) 0))
		|(Neg,Neg)			-> if lt (abs x) (abs y) then (NonNeg, (subs (listof y) (listof x) 0) ) else (Neg, (subs (listof x) (listof y) 0))
		;; 
(* IT IS A SUB PART OF MULTIPLY WITH IS MULTIPLYING WHOLE LIST BY A INTEGER NO. N *)
let rec nmult l n c= match l with
        [] -> if c=0 then [] else [c]
				| x::xs -> [(n*x+c) mod 10] @ nmult xs n ((n*x+c)/10);;  
(* THIS FUNCTION IS RESPONSIBLE FOR MULTIPLYING TWO LIST L1 AND L2 USING NMULT   *)
let rec multiply l1 l2 =  match (l1,l2) with
        ([],[])         -> [] 
        |([],y::ys)     -> []
        |(x::xs,[])     ->  []
        |(l1,y::ys)  -> [0] @ rev (adde (rev (nmult l1 y 0)) (rev (multiply l1 ys)))
				;;      

(*THIS FUNCTION IS ONLY REMOVING ONE ADDITIONAL ZERO AND REPORINTG FINAL ANS TO MULT *)				
let multi l1 l2 =  match multiply ( rev l1 ) ( rev l2 ) with
        [] -> []
				| x::xs -> rev xs;;   
(*THIS IS FUNCTION MULT WHAT IT MEAN TO YOU WITH CHECKING ALL CASES*)
let mult x y = match (signof x,signof y) with
				(Neg,NonNeg) 		-> (Neg,multi (listof x) (listof y))
				|(NonNeg,Neg)		-> (Neg,multi (listof x) (listof y))
				|(NonNeg,NonNeg) -> (NonNeg,multi (listof x) (listof y))
				|(Neg,Neg)			-> (NonNeg,multi (listof x) (listof y))
				;; 
(*THIS IS CONVERTING WHOLE LIST INTO A STRING SUB PART OF PRINT_NUM  *)
let rec clist l = match l with
		[] -> ""
		|x::xs-> (string_of_int x) ^ clist xs;; 
(*THIS FUNCTION IS JUST CONVERTING BIGINT TO STIRNG   *)
let print_num x = 
    if (signof x) = Neg then ("-" ^ clist (listof x))
		else clist ( listof x )
		;;
(*THIS FUNCTION IS CONVERTING AN INTEGER TO A LIST   *)
let rec makel x = match x with
		0 -> []
		| _ -> (makel (x/10))@[x mod 10];;
(*THIS FUNCTION IS  WHAT IT MEAN TO YOU WITH ENSURING THE SIGNATURE  *)
let mk_big x = 
	if x<0 then (Neg,makel (-1*x))
	else	(NonNeg,makel x)
	;;
(*THIS FUNCITON DOING DIVISION USEING BASIC METHOD OF DIV MEANS SUBTRACTING N1 BY N2 AGAIN AND AGGIN AND 
COUNTING THE TOTAL EVALUTION AND REPORINT THAT IT HAS NOT S GOOD EFFICIANCY  *)
let rec div1 n1 n2 = match (n1,n2) with
		((_,_),(_,[])) -> raise Division_By_Zero
	|   ((_,[]),(_,_))   -> (NonNeg,[])
	|   ((Neg,_),(NonNeg,_)) -> (minus (div1 (minus n1) n2))
	|   ((NonNeg,_),(Neg,_)) -> (minus (div1 n1 (minus n2)))
	|   ((Neg,_),(Neg,_)) ->  div1 (minus n1) (minus n2)
	|   ((NonNeg,_),(NonNeg,_)) -> if (lt  n1  n2 ) then (mk_big 0) 
										else  (add (mk_big 1) (div1 (sub  n1  n2 )  n2 ))
		;;	
(*IT IS USING BASIC RULE OF NO. SYSTEM A = BQ + R HERE R IS REMANDER AND Q IS QUITIENT ON DIVIDING A BY B  *)
let rem1 n1 n2 = sub n1 (mult (div1 n1 n2) n2);;  

let divi l1 l2 = listof (div1 (NonNeg,l1) (NonNeg,l2));;
let remi l1 l2 = listof (rem1 (NonNeg,l1) (NonNeg,l2)) ;;
let rec split l n = match l with
    [] -> []
    |x::xs -> if n >= 1 then [x] @split xs (n-1)
                else  []
                ;;
let splitHd l n = (split l n);;
let splitTl l n = List.rev (split (List.rev l) n);; 
let rec append0 l n = if (n >= 1) then append0 (l @ [0]) (n-1)
											else l;;
let rec devil alist blist q = match blist with
    [] -> q
    |_ -> if (greater alist blist)  then q
            else if (length alist = length blist) then (q @ (divi blist alist))
            else (
                let l = (splitHd blist ((List.length alist)+1)); in      
                let	qn = divi l alist;  in                         
                let	rn = remi l alist;  in
                    let	ln = splitTl blist ((List.length blist)-(List.length l)); in
			            q @ devil alist (rn @ ln) (append0 qn ((length alist)-(length rn))) 
								)
                        ;; 
let rec remil alist blist q = match blist with
[] -> [0]
|_ -> if (greater alist blist)  then blist
        else if (length alist = length blist) then (remi blist alist)
        else (
            let l = (splitHd blist ((List.length alist)+1)); in      
            let	qn = divi l alist;  in                         
            let	rn = remi l alist;  in
            let	ln = splitTl blist ((List.length blist)-(List.length l)); in
                    remil alist (rn @ ln) (q @ (append0 qn ((length alist)-(length rn)))) 
                    )
                    ;;                         
let div x y = match (signof x,signof y) with
        (Neg,NonNeg) 		-> (Neg,devil (listof y) (listof x) [])
        |(NonNeg,Neg)		-> (Neg,devil (listof y) (listof x) [])
        |(NonNeg,NonNeg)    -> (NonNeg,devil (listof y) (listof x) [])
        |(Neg,Neg)			-> (NonNeg,devil (listof y) (listof x) [])
        ;;
let rem x y = match (signof x,signof y) with
        (Neg,NonNeg) 		-> (NonNeg,remil (listof y) (listof x) [])
        |(NonNeg,Neg)		-> (Neg,remil (listof y) (listof x) [])
        |(NonNeg,NonNeg)    -> (NonNeg,remil (listof y) (listof x) [])
        |(Neg,Neg)			-> (Neg,remil (listof y) (listof x) [])
				;;    
(* THANKS HERE IS A LATE SUBMISSION AND I AM MAILING IT TO YOU BECOUSE I THOUGHT THAT LINK WOULD NE GONE AND
 I CAN'T SUMMIT IT AFTER THET SO I DID CONFERIM SUBMISSION IN JUST HALF CODE. IT IS FULL WORKING CODE
 Minor View
	inducation/type inferensing/
	integer part :(0)|(1-9)(0-9)*
	dot: "."
	fractional part :([0-9]*[1-9]+)*
 fractional part: [1-9]([1-9]*([0]+[1-9]* )*
 final ans : (+/-)? ([1-9][0-9]*|0).[0-9]([1-9](0+]1-9]*. *)
