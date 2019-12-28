open A1


(* cutting : function takes a input list and return a boolean value wether it has at least one ("invalid",Tbool) tuple or not 
                val cutting :  ((string * exptype) list) -> bool
                *)
let rec cutting l = match l with
        [] -> true
        |x::xs -> if x = ("invalid",Tbool) then false else cutting xs;;
(* check : function takes a input element and a input list and return a boolean value wether it has it or not 
                val check :  (string*exptype) -> ((string * exptype) list) -> bool
                *)     
let rec check y l = match l with
        [] -> false
        |x::xs -> match y with (str,_) -> match x with (s,_) -> if(str = s) then true else check y xs;;
(* extraremove : function takes two input list and return a second list on lf which has all the elements of l except dublicates of l 
                val extraremove :  ((string * exptype) list) -> ((string * exptype) list)
                *)
let rec extraRemove l lf = match l with
                [] -> lf
                |x::xs -> if check x lf then extraRemove xs lf else extraRemove xs (lf@[x]) ;;


(* searchInTeble : function takes a string which it wants to search in its input list and then returns a (type,bool) tuple 
                val extraremove :  string -> ((string * exptype) list) -> (type,bool)
                *)
let rec searchInTable str g = match g with
				[] -> (Tbool,false);
                | x::xs -> match x with (astr,t) -> if(astr = str) then (t,true) else searchInTable str xs ;;

(* getType : this function is recursivly try to determine the type of given expression 'e' with the help of given table 'g'
             if it succceed then it returns (type,true) value tuple and if it not then in all possible case of error occurrence it will returns (Tbool,false)
             this true and false corresponds to the flag wether the tuple which returning during ans has a valid ans or not
             val getType : exptree -> ((string * exptype) list) -> (exptype*bool) 
             *)
let rec getType ex g =  match ex with
		Var(x)                  	->  searchInTable x g
        | N(m)                  	->	(Tint,true)
        | B(m)                  	->	(Tbool,true)
		| Abs  (t)              	->	if (getType t g) = (Tint,true) then (Tint,true) else (Tbool,false)
        | Negative (t)          	->	if (getType t g) = (Tint,true) then (Tint,true) else (Tbool,false)
		| Not (t)               	->  if (getType t g) = (Tbool,true) then (Tbool,true) else (Tbool,false)
		| Add  (t1,t2)          	->	if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tint,true) else  (Tbool,false)	
        | Sub  (t1,t2)          	->  if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tint,true) else  (Tbool,false)
        | Mult (t1,t2)          	->	if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tint,true) else  (Tbool,false)
        | Div  (t1,t2)          	->	if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tint,true) else  (Tbool,false)
		| Rem  (t1,t2)          	->	if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tint,true) else  (Tbool,false)
        | Conjunction  (t1,t2)  	->  if ((getType t1 g = (Tbool,true))&&(getType t2 g = (Tbool,true))) then (Tbool,true) else  (Tbool,false)
        | Disjunction  (t1,t2)  	->	if ((getType t1 g = (Tbool,true))&&(getType t2 g = (Tbool,true))) then (Tbool,true) else  (Tbool,false)
        | Equals    (t1,t2)     	->	if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tbool,true) else  (Tbool,false)
        | GreaterTE (t1,t2)     	->  if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tbool,true) else  (Tbool,false)
        | LessTE    (t1,t2)     	->  if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tbool,true) else  (Tbool,false)
        | GreaterT  (t1,t2)     	->  if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tbool,true) else  (Tbool,false)
        | LessT     (t1,t2)     	->  if ((getType t1 g = (Tint,true))&&(getType t2 g = (Tint,true))) then (Tbool,true) else  (Tbool,false)
        | InParen (t)           	->  getType t g
        | IfThenElse(t1,t2,t3)  	->  if (getType t1 g = (Tbool,true)) then (if (getType t2 g = getType t3 g) then (getType t2 g) else (Tbool,false)) 
                                        else (Tbool,false)	
        | Tuple (n,expl)        	->	let rec get_tuple_type_list l = match l with 
                                        [] -> [] 
                                        |t::ts ->   let ry = match (getType t g) with (t,true) -> [t] @ get_tuple_type_list ts;
                                                                                    | (_,false) -> get_tuple_type_list ts; in ry in
                                        let lt = get_tuple_type_list expl; in
                                            if List.length lt = n then (Ttuple(lt),true) else (Tbool,false);
		| Project ((i,n),t)     	->	let rec get_proj_type count l = match l with 
												[] -> (Tbool,false)
												|t::ts -> if count = i then (t,true) else get_proj_type (count+1) ts ; in
										let a = match (getType t g) with  
											(Ttuple(l),true) -> get_proj_type 1 l
											| _ -> (Tbool,false) in a;
        | Let(d,t)				    ->	let rec find_DefType defination table = match defination with
                                                    SimpleWT(str,e) -> let r = match getType e table with (typp,true) -> [(str,typp)]
                                                                                                        |_ -> [("invalid",Tbool)] in r;
                                                    |SimpleRec(col,e) ->  let rt = match col with Colon(str,tp) -> match getType e ([(str,tp)]@table) with 
                                                                                                                (typp,true) -> [(str,typp)] 
                                                                                                                |_ -> [("invalid",Tbool)] in rt; 
                                                    |Simple(col,e) -> let rt = match col with Colon(str,tp) -> if(getType e table = (tp,true)) then [(str,tp)]
                                                                                                        else [("invalid",Tbool)] in rt;
                                                    |Sequence(l) -> let rec seq defl table = match defl with 
                                                                    [] -> []
                                                                    |d::ds -> let lt = find_DefType d table in (seq ds (lt@table) ) @ lt;in
                                                                     (extraRemove (seq l table) []);  
                                                    |Parallel(l) -> let rec par defl = match defl with 
                                                                    [] -> []
                                                                    |d::ds -> (par ds) @ (find_DefType d table) ; in let lt = par l in
                                                                    if List.length (extraRemove lt []) = List.length lt then lt else [("invalid",Tbool)]; 
                                                    |Local(d1,d2) -> (find_DefType d2   ((find_DefType d1 table)@ table)); in
                                                    if(cutting (find_DefType d g)) then getType t ((find_DefType d g)@g) else (Tbool,false);
        | FunctionAbstraction(col,t)	->  let rt = match col with Colon(str,gtp) -> 
                                                        let a = match getType t ([(str,gtp)]@g) with 
                                                                            (typp,true) -> (Tfunc(gtp,typp),true)
                                                                            |(Tbool,false)  -> (Tbool,false) in a in rt;
        | FunctioinAbsrationWT(s,t)	->  let a = match getType t g with
                                                    (typp,true) -> let r = match searchInTable s g with 
                                                                            (typw,true) -> (Tfunc(typw,typp),true)
                                                                            |(Tbool,false)  -> (Tbool,false) in r;
                                                    |(Tbool,false)  -> (Tbool,false)  in a;                                                                    
        | FunctionCall(t1,t2)		->  match  getType t1 g with
                                        (_,false) -> (Tbool,false)
                                        |(typp,true) ->  match typp with
                                                    Tfunc(alfha1,alfha2) -> if ((getType t2 g) = (alfha1,true)) then (alfha2,true) else (Tbool,false)
                                                    | _ -> (Tbool,false);;   
(* this function uses gettype for working as you mentioned *)
(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let hastype g e typ = if (getType e g) = (typ,true) then true else false;;


(* find_DefType : this function developed recursively on defination for findout new table corresponds to input defination d with the help of given table
    val : find_Deftype : defination ->   ((string * exptype) list) -> ((string * exptype) list)  
*)
let rec find_DefType defination table = match defination with
        SimpleWT(str,e) -> let r = match getType e table with (typp,true) -> [(str,typp)]
                                                            |_ -> [("invalid",Tbool)] in r;
        |Simple(col,e) -> let rt = match col with Colon(str,tp) -> if(getType e table = (tp,true)) then [(str,tp)]
                                                            else [("invalid",Tbool)] in rt;
        |Sequence(l) -> let rec seq defl table = match defl with 
                                [] -> []
                                |d::ds -> let lt = find_DefType d table in (seq ds (lt@table) ) @ lt;in (extraRemove (seq l table) []);  
        |Parallel(l) -> let rec par defl = match defl with 
                                [] -> []
                                |d::ds -> (par ds) @ (find_DefType d table) ; in let lt = par l in
                                if List.length (extraRemove lt []) = List.length lt then lt else [("invalid",Tbool)]; 
        |Local(d1,d2) -> (find_DefType d2   ((find_DefType d1 table)@ table));;


(* this function also works as like check which mentiond earlier on the top with slight difference that it compares exactly equal thing  *)
let rec checkDef y l = match l with
        [] -> false
        |x::xs -> if(y=x) then true else check y xs;;

(* this function give true false wether this two set exctly same or not  *)
let rec funcMatch l1 l2 = match l1 with
        [] -> true
        |y::ys -> if(checkDef y l2) then funcMatch ys l2 else false ;;        
(* it implements using the find def type function mentioned earlier *)
(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let yields g d g_dash = let lt = find_DefType d g in
                            if(cutting lt) then
                                if(List.length lt = List.length g_dash ) then funcMatch lt g_dash else false
                            else false ;;                                             