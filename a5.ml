open A1
(* closure => expression*table(valClosure) *)
val krivineMc : (exptree*(string*((value*(string*answer)) list))) -> (exptype*bool)
val getval : exptree -> ((string * value) list) -> (value) 


let rec krivineMc closure stack= match clouser with
			(FunctionAbstraction(s,e),g) -> ( (getval e g),stack )
			|(FunctionCall(e1,e2),g) -> ((getval e1 g,g),([(e2,g)]@table))
																	
			|(e,g)  -> ((getval e g),g)