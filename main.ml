(* tipi di base del mio linguaggio *)
type types = 
	| IntType 
	| BoolType 
	| StringType
	| SetType of types
(* evaluation types del mio linguaggio *)
type ide = string;;
type evT = 
	| Int of int
	| Bool of bool
	| String of string
	| Set of types*evT list
	| Unbound;;
(* Albero di sintassi astratta *)
type exp = 
	| Den of ide
	| CstInt of int
	| CstBool of bool
	| CstString of string
	| Sum of exp*exp
	| Let of ide*exp*exp
	| EmptySet of types
	| Singleton of exp*types
	| Insert of exp*exp (* Set*evT *)
	| Remove of exp*exp (* Set*evT *)
	| IsEmpty of exp
	| SetMax of exp
	| SetMin of exp
	| IsIn of exp*exp (* Set*evT *)
	| IsSubsetOf of exp*exp (* setContenitore*setContenuto *)
	| Union of exp*exp (* set1 + set2 *)
	| Intersect of exp*exp (* set1 and set2 *)
	| Difference of exp*exp;; (* set1 - set2 *)

(* Definizione del typechecking dinamico 

	typecheck è una funzione che, attraverso il pattern matching,
	restituisce vero se il tipo del valore fornito (actualType) 
	coincide con il tipo atteso (typeDescriptor), falso altrimenti
	
	getType è una funzione che, dato un valore, restuisce il tipo
	di tale valore
*)
let typecheck (typeDescriptor: types) (actualType: evT): bool =
	match (typeDescriptor,actualType) with
	| (IntType, Int(_)) -> true
	| (BoolType, Bool(_)) -> true
	| (StringType, String(_)) -> true
	| (SetType(setFormerType), Set(setActualType,_)) -> 
		if setFormerType = setActualType then true else false
	| _ -> false;;
let getType (value: evT): types = 
	match value with
	| Int(_) -> IntType
	| Bool(_) -> BoolType
	| String(_) -> StringType
	| Set(typeOfSet, _) -> SetType(typeOfSet)
	| Unbound -> failwith("Unbound value has no type");;
(* Definizione dell'ambiente 

	L'ambiente è stato implementato sotto forma di lista,
	dove ogni elemento contiene il binding tra l'identificatore
	e il valore.
*)
type 't env = (ide * 't) list;;
let emptyEnv : evT env = [];;
let bind (env: evT env) (id: ide) (value: evT) = (id,value)::env;;
let rec lookup (myEnv: evT env) (x: ide) : evT= 
	match myEnv with
	| (y,v)::t -> if x = y then v else lookup t x
	| [] -> Unbound;;

(* Implementazione delle funzioni legate al tipo Set *)
let emptyEvtList : evT list = [];; 
let emptySet (typeOfSet: types): evT = 
	match typeOfSet with
	| SetType(_) -> failwith("A set cannot cointain another set")
	| _ -> Set(typeOfSet,emptyEvtList);; 
let singletonSet (v: evT) (setType: types): evT = 
	let valueType = getType v in (* controllo il tipo del valore passato *)
		match valueType with
		| SetType(_) -> failwith("A set cannot cointain another set")
		| _ -> if setType = valueType then Set(setType, v::emptyEvtList) (* creo il set solo se il tipo del valore ed il tipo specificato per il set coincidono *)
				else failwith("Set type and value type don't match");; 
let bool_IsEmpty (value: evT): bool = 
	match value with
	| Set(_,[]) -> true
	| Set(_,head::tail) -> false
	| _ -> failwith("This is not a set")
let set_setInsertion (set: evT) (value: evT): evT =
	let valueType = getType(value) in (* controllo il tipo del valore passato *)
		let expectedSetType = SetType(valueType) in
			match typecheck expectedSetType set with 
			| true -> (
						match set with 
						| Set(typeOfSet,[]) -> Set(typeOfSet, [value])
						| Set(typeOfSet,list) -> (* se il set non è vuoto, controllo se contiene già value usando List.mem *)
							if List.mem value list then failwith("The value is already into the set")
							else Set(typeOfSet, value::list)
					  )
			| false -> failwith("Value and set type don't match");;
let set_setRemove (set: evT) (value: evT): evT =
	let valueType = getType(value) in
		let expectedSetType = SetType(valueType) in
			match typecheck expectedSetType set with
			| true -> (
						match set with
						| Set(_, []) -> failwith("Trying to remove an element from an empty set")
						| Set(typeOfSet,list) ->
							let aux = fun x -> x <> value in
								Set(typeOfSet, List.filter aux list)
								(* List.filter mi restituisce la lista di elementi che sono diversi
								   dal valore che voglio rimuovere *)
					  )
			| false -> failwith("Value and set type don't match");;
let evT_findMax (v: evT) : evT =
	match getType(v) with
	| SetType(_) ->	match v with 
					| Set(_, []) -> failwith("The set is empty")
					| Set(_,head::tail) -> (
							(* Uso una funzione ricorsiva ausiliaria per fare uno
							   scan lineare del set e trovare il valore massimo *)
							let rec aux list localMax = 
								match list with
								| [] -> localMax
								| h::t -> if h > localMax then aux t h
										  else aux t localMax
							in aux tail head
						   ) 
	| _ -> failwith("The parameter is not a set");;
let evT_findMin (v: evT) : evT =
	match getType(v) with
	| SetType(_) ->	match v with 
					| Set(_, []) -> failwith("The set is empty")
					| Set(_,head::tail) -> (
							(* Come sopra, uso una funzione ausiliaria per trovare
							   il valore minimo nel set *)
							let rec aux list localMin = 
								match list with
								| [] -> localMin
								| h::t -> if h < localMin then aux t h
										  else aux t localMin
							in aux tail head
						   ) 
	| _ -> failwith("The parameter is not a set");;
let bool_isInSet (set: evT) (value: evT): bool = 
	let valueType = getType(value) in
		let expectedSetType = SetType(valueType) in
			match typecheck expectedSetType set with
			| true -> ( match set with
						| Set(_,[]) -> false
						| Set(_,list) -> List.mem value list
						(* List.mem restituisce vero se il valore
						   è contenuto nella lista, falso altrimenti *)
					  )
			| false -> failwith("Value and set type don't match");;
let bool_isSubsetOf (set1: evT) (set2: evT): bool = 
	match (set1,set2) with (* Controllo che entrambi siano set e che siano dello stesso tipo *)
	| (Set(type1, list1),Set(type2, list2)) -> if type1 <> type2 then failwith("Type mismatch")
								 else (
								 	(* La funzione ausiliaria inInList1, applicata ad un parametro, 
								 	   restituisce vero se tale parametro è contenuto in list1, 
								 	   falso altrimenti.
								 	   La funzione List.for_all verifica se tutti gli elementi
								 	   in list2 verificano la proprietà espressa da isInList1 *)
								 	let isInList1 = fun elem -> List.mem elem list1 
								 		in List.for_all isInList1 list2
								 )
	| (Set(_), _) -> failwith("Second element is not a set")
	| (_, Set(_)) -> failwith("First element is not a set")
	| (_,_) -> failwith("Both elements are not sets")
let set_unionSet (set1: evT) (set2: evT): evT = 
	match (set1,set2) with (* Controllo che entrambi siano set e che siano dello stesso tipo *)
	| (Set(type1, list1),Set(type2, list2)) -> if type1 <> type2 then failwith("Type mismatch")
												else(
													(*
														La funzione ausiliaria isNotInList1 prende un parametro 
														e restituisce vero se il parametro non è contenuto in 
														list1, falso altrimenti.
														Per effettuare l'unione degli insiemi viene quindi 
														utilizzata la funzione di base List.filter per raccogliere
														nella lista cleanedList gli elementi di list2 che NON 
														sono già presenti in list1; alla fine viene restituito un 
														set contenente gli elementi di list1 e cleanedList. 
													*)
													let isNotInList1 = fun elem -> not (List.mem elem list1)
														in let cleanedList2 = List.filter isNotInList1 list2 in
															Set(type1, List.append list1 cleanedList2)
												)
	| (Set(_), _) -> failwith("Second element is not a set")
	| (_, Set(_)) -> failwith("First element is not a set")
	| (_,_) -> failwith("Both elements are not sets");;

let set_intersectSet (set1: evT) (set2: evT): evT = 
	match (set1,set2) with (* Controllo che entrambi siano set e che siano dello stesso tipo *)
	| (Set(type1, list1),Set(type2, list2)) -> if type1 <> type2 then failwith("Type mismatch")
												else(
													(*
														Utilizzando la funzione ausiliaria isInList1, viene 
														restituito un set contenente gli elementi di list2
														che sono presenti anche in list1
													*)
													let isInList1 = fun elem -> List.mem elem list1
														in let commonElementsList = List.filter isInList1 list2 in
															Set(type1, commonElementsList)
												)
	| (Set(_), _) -> failwith("Second element is not a set")
	| (_, Set(_)) -> failwith("First element is not a set")
	| (_,_) -> failwith("Both elements are not sets");;
	
let set_differenceSet (set1: evT) (set2: evT): evT = 
	match (set1,set2) with (* Controllo che entrambi siano set e che siano dello stesso tipo *)
	| (Set(type1, list1),Set(type2, list2)) -> if type1 <> type2 then failwith("Type mismatch")
												else(
													(*
														Utilizzando la funzione ausiliaria isNotInList2, viene
														restituito un set contenente gli elementi di list1
														che non sono presenti in list2
													*)
													let isNotInList2 = fun elem -> not (List.mem elem list2)
														in let differenceElementsList = List.filter isNotInList2 list1 in
															Set(type1, differenceElementsList)
												)
	| (Set(_), _) -> failwith("Second element is not a set")
	| (_, Set(_)) -> failwith("First element is not a set")
	| (_,_) -> failwith("Both elements are not sets");;

(* implementazione delle funzioni di base del linguaggio *)
let int_sum (v1: evT) (v2: evT) : evT =
	match (typecheck IntType v1, typecheck IntType v2) with
	| (true,true) -> ( match (v1,v2) with
					 | (Int(x),Int(y)) -> Int(x+y)
					 | _ -> failwith("run-time error") )
	| (_,_) -> failwith("Type error in Sum");;
	
(* Funzione di valutazione delle espressioni *)
let rec eval (expression: exp) (myEnv: evT env): evT =
	match expression with
	| Den(id) -> lookup myEnv id
	| CstInt(i) -> Int(i)
	| CstBool(b) -> Bool(b)
	| CstString(s) -> String(s)
	| Sum(e1,e2) -> int_sum (eval e1 myEnv) (eval e2 myEnv)
	| Let(id,e,eBody) -> 
		let newEnv = bind myEnv id (eval e myEnv) in 
			eval eBody newEnv
	| EmptySet(setType) -> emptySet setType
	| Singleton(e1,setType) -> singletonSet (eval e1 myEnv) setType
	| Insert(e1,e2) -> set_setInsertion (eval e1 myEnv) (eval e2 myEnv)
	| Remove(e1,e2) -> set_setRemove (eval e1 myEnv) (eval e2 myEnv)
	| IsEmpty(den) -> Bool(bool_IsEmpty (eval den myEnv))
	| SetMax(e) -> evT_findMax (eval e myEnv)
	| SetMin(e) -> evT_findMin (eval e myEnv)
	| IsIn(e1,e2) -> Bool(bool_isInSet (eval e1 myEnv) (eval e2 myEnv))
	| IsSubsetOf(e1,e2) -> Bool(bool_isSubsetOf (eval e1 myEnv) (eval e2 myEnv))
	| Union(e1,e2) -> set_unionSet (eval e1 myEnv) (eval e2 myEnv)
	| Intersect(e1,e2) -> set_intersectSet (eval e1 myEnv) (eval e2 myEnv)
	| Difference(e1,e2) -> set_differenceSet (eval e1 myEnv) (eval e2 myEnv);;
