(* tipi di base del mio linguaggio *)
type types = 
	| IntType 
	| BoolType 
	| StringType
	| SetType of types
type ide = string;;
type 't env = (ide * 't) list;;
(* Albero di sintassi astratta *)
type exp = 
	| Den of ide
	| CstInt of int
	| CstBool of bool
	| CstString of string
	| Sum of exp*exp
	| Equal of exp*exp
	| Not of exp
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
	| Difference of exp*exp (* set1 - set2 *)
	| Fun of ide*exp (* Dichiarazione di funzione*)
	| Apply of exp*exp (* Applicazione di funzione *)
	| ForAll of exp*exp (* functionIde*set *)
	| Exists of exp*exp (* functionIde*set *)
	| Map of exp*exp  (* functionIde*set *)
	| Filter of exp*exp;; (* functionIde*set *)
	
	
(* evaluation types del mio linguaggio 
	
	Le funzioni sono state implementate prevedendo lo 
	scoping statico, seguendo questo schema:
   
   	Closure(argIde,fBody,fEnv)
		- nome del parametro formale (argIde)
		- corpo della funzione (fBody)
		- ambiente al momento della dichiarazione (fEnv)
		
   	Apply rappresenta l'effettiva chiamata della funzione, dove
   	il primo parametro è la denominazione della funzione chiamata, 
   	mentre il secondo parametro è il parametro attuale
*)
type evT = 
	| Int of int
	| Bool of bool
	| String of string
	| Set of types*evT list
	| Closure of ide*exp*evT env 
	| Unbound;;
(* 
	getType è una funzione che, dato un valore, restuisce il tipo
	di tale valore
*)
let getType (value: evT): types = 
	match value with
	| Int(_) -> IntType
	| Bool(_) -> BoolType
	| String(_) -> StringType
	| Set(typeOfSet, _) -> SetType(typeOfSet)
	| Closure(_) -> failwith("Closure value has no type")
	| Unbound -> failwith("Unbound value has no type");;
(* Definizione dell'ambiente 

	L'ambiente è stato implementato sotto forma di lista,
	dove ogni elemento contiene il binding tra l'identificatore
	e il valore.
*)
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
		| _ -> (* creo il set solo se il tipo del valore ed il tipo specificato per il set coincidono *)
					if setType = valueType then Set(setType, v::emptyEvtList) 
					else failwith("Set type and value type don't match");; 

let bool_IsEmpty (value: evT): bool = 
	match value with
	| Set(_,[]) -> true
	| Set(_,head::tail) -> false
	| _ -> failwith("The parameter is not a set");;

let set_setInsertion (set: evT) (value: evT): evT =
	let valueType = getType(value) in (* controllo il tipo del valore passato *)
		match set with 
			| Set(typeOfSet,lst) -> if typeOfSet = valueType then(
																	(* se il set non è vuoto, controllo se contiene già value usando List.mem *)
																	if List.mem value lst then failwith("The value is already into the set")
																	else Set(typeOfSet, value::lst)
															 ) else failwith("Value and set type don't match")
			| _ -> failwith("The parameter is not a set");;

let set_setRemove (set: evT) (value: evT): evT =
	let valueType = getType(value) in
		match set with
			| Set(typeOfSet,lst) -> if typeOfSet = valueType then(
																	if lst = emptyEvtList then failwith("Trying to remove an element from an empty set")
																	else(
																		let aux = fun x -> x <> value in
																		Set(typeOfSet, List.filter aux lst)
																		(* List.filter mi restituisce la lista di elementi che sono diversi
																			 dal valore che voglio rimuovere *)
																	)
															 )
															 else failwith("Value and set type don't match")
			| _ -> failwith("The parameter is not a set");;

let evT_findMax (v: evT) : evT =
	match v with
	| Set(_, []) -> failwith("The set is empty")
	| Set(_,head::tail) -> (
			(* Uso una funzione ricorsiva ausiliaria per fare uno
			   scan lineare del set e trovare il valore massimo *)
					let rec aux lst localMax = 
						match lst with
						| [] -> localMax
						| h::t -> if h > localMax then aux t h
									else aux t localMax
					in aux tail head
		   ) 
	| _ -> failwith("The parameter is not a set");;

let evT_findMin (v: evT) : evT =
	match v with
	| Set(_, []) -> failwith("The set is empty")
	| Set(_,head::tail) -> (
			(* Uso una funzione ricorsiva ausiliaria per fare uno
			   scan lineare del set e trovare il valore minimo *)
					let rec aux lst localMin = 
						match lst with
						| [] -> localMin
						| h::t -> if h < localMin then aux t h
									else aux t localMin
					in aux tail head
		   ) 
	| _ -> failwith("The parameter is not a set");;

let bool_isInSet (set: evT) (value: evT): bool = 
	let valueType = getType(value) in
		match set with
		| Set(typeOfSet, lst) -> if typeOfSet = valueType then List.mem value lst
															(* List.mem restituisce vero se il valore
						   									 è contenuto nella lista, falso altrimenti *)
														 else failwith("Value and set type don't match")
		| _ -> failwith("The parameter is not a set");;
			
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
	| (Set(_), _) -> failwith("Second parameter is not a set")
	| (_, Set(_)) -> failwith("First parameter is not a set")
	| (_,_) -> failwith("Both parameters are not sets")
	
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
	| (Set(_), _) -> failwith("Second parameter is not a set")
	| (_, Set(_)) -> failwith("First parameter is not a set")
	| (_,_) -> failwith("Both parameters are not sets");;

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
	| (Set(_), _) -> failwith("Second parameter is not a set")
	| (_, Set(_)) -> failwith("First parameter is not a set")
	| (_,_) -> failwith("Both parameters are not sets");;
	
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
	| (Set(_), _) -> failwith("Second parameter is not a set")
	| (_, Set(_)) -> failwith("First parameter is not a set")
	| (_,_) -> failwith("Both parameters are not sets");;

(* implementazione delle funzioni di base del linguaggio *)
let int_sum (v1: evT) (v2: evT) : evT =
	match (v1, v2) with
	| (Int(x),Int(y)) -> Int(x+y)
	| (_,_) -> failwith("Type error in Sum");;

let bool_equal (v1: evT) (v2: evT) : bool =
	match (v1,v2) with
	| (Int(x), Int(y)) -> x = y
	| (Bool(x), Bool(y)) -> x = y
	| (String(x), String(y)) -> x = y
	| (Closure(_), Closure(_)) -> failwith("Functions can't be compared")
	| (Set(_), Set(_)) -> failwith("Sets can't be compared")
	| (_,_) -> failwith("Type mismatch")
	
let bool_not (v: evT) : bool =
	match v with
	| Bool(x) -> not x
	| _ -> failwith("Type error in not");;
	
(**)
let getExpType (expression: exp): types =
	match expression with
	| Den(_) -> StringType
	| CstInt(_) -> IntType
	| CstBool(_) -> BoolType
	| Sum(_,_) -> IntType
	| Equal(_,_) -> BoolType
	| _ -> IntType;;



(* Funzione di valutazione delle espressioni *)
let rec eval (expression: exp) (myEnv: evT env): evT =
	match expression with
	| Den(id) -> lookup myEnv id
	| CstInt(i) -> Int(i)
	| CstBool(b) -> Bool(b)
	| CstString(s) -> String(s)
	| Sum(e1,e2) -> int_sum (eval e1 myEnv) (eval e2 myEnv)
	| Equal(e1,e2) -> Bool(bool_equal (eval e1 myEnv) (eval e2 myEnv) )
	| Not(e) -> Bool(bool_not (eval e myEnv))
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
	| Difference(e1,e2) -> set_differenceSet (eval e1 myEnv) (eval e2 myEnv)
	| Fun(argIde,funExp) -> Closure(argIde,funExp,myEnv)
	| Apply(_)
	| ForAll(_) 
	| Exists(_)
	| Filter(_)
	| Map(_) -> (* evT_funApplication applica il parametro attuale actualValue alla 
				    funzione functionClosure, restituendo il risultato della valutazione *)
					let evT_funApplication (functionClosure: evT) (actualValue: evT): evT = 
					(
						match functionClosure with (* chiusura non ricorsiva *)
						| Closure(argIde,fBody,fEnv) -> 
							(* il parametro attuale è già stato valutato nell'ambiente locale del chiamante *)
							let functionEnv = (bind fEnv argIde actualValue) in eval fBody functionEnv  
							(* aggiungo il par. attuale all'ambiente locale del chiamato :
							   espandiamo l'ambiente della funzione con il binding del parametro 
							   attuale ed eseguiamo il corpo della funzione*)
						| _ -> failwith("Function not found")
					) in
						match expression with
						| Apply(e1,e2) -> evT_funApplication (eval e1 myEnv) (eval e2 myEnv)
						| ForAll(e1,e2) -> 
							let bool_forAllFunction (functionClosure: evT) (set: evT): bool =
								(
									match functionClosure with (* controllo la correttezza dei parametri *)
									| Closure(argIde,fBody,fEnv) -> (
																										match set with
																										| Set(type1, lst) -> 
																											(* uso una funzione ausiliaria che applica ricorsivamente
																												 il predicato a ciascun elemento del set
																												 La funzione mette in and i risultati delle valutazioni parziali
																												 e quindi, se la valutazione di un valore producesse un valore 
																												 falso, si potrebbe subito evitare di dover valutare gli elementi
																												 successivi.
																											 *)
																											let rec applyForAll lista  =
																											(
																												match lista with
																												| [] -> true
																												| head::tail ->	let newValue = evT_funApplication functionClosure head in
																																	(
																																		match newValue with
																																		| Bool(true) -> applyForAll tail
																																		| Bool(false) -> false
																																		| _ -> failwith("Result of the function is not a bool")
																																	) 
																											) 
																											in applyForAll lst
																										| _ -> failwith("Second argument is not a set")
																									)
									| Unbound -> failwith("Function not found")
									| _ -> failwith("Runtime Error")
								) in Bool(bool_forAllFunction (eval e1 myEnv) (eval e2 myEnv))	
						| Exists(e1,e2) -> 
							let bool_existsFunction (functionClosure: evT) (set: evT): bool =
								(
									match functionClosure with (* controllo la correttezza dei parametri *)
									| Closure(argIde,fBody,fEnv) -> (
																										match set with
																										| Set(type1, lst) -> 
																											(* uso una funzione ausiliaria che applica ricorsivamente
																												 il predicato a ciascun elemento del set
																												 La funzione mette in or i risultati delle valutazioni parziali
																												 e quindi, se la valutazione di un valore producesse un valore 
																												 vero, si potrebbe subito evitare di dover valutare gli elementi
																												 successivi.
																											 *)
																											let rec applyExist lista  =
																											(
																												match lista with
																												| [] -> false
																												| head::tail ->	let newValue = evT_funApplication functionClosure head in
																																	(
																																		match newValue with
																																		| Bool(true) -> true
																																		| Bool(false) -> applyExist tail
																																		| _ -> failwith("Result of the function is not a bool")
																																	) 
																											) 
																											in applyExist lst
																										| _ -> failwith("Second argument is not a set")
																									)
									| Unbound -> failwith("Function not found")
									| _ -> failwith("Runtime Error")
								) in Bool(bool_existsFunction (eval e1 myEnv) (eval e2 myEnv))	
						| Filter(e1,e2) ->
							let set_filterFunction (funClosure: evT) (set: evT): evT =
							(
								match funClosure with (* controllo la correttezza dei parametri *)
								| Closure(argIde, fBody, fEnv) -> (
																										match set with
																										| Set(type1, lst) ->
																												(*
																														applyFilter è una funzione  ricorsiva ausiliaria che applica
																														il predicato a tutti gli elementi della lista passata come parametro.
																														Se un valore della lista soddisfa il predicato allora viene salvato 
																														in una lista che funge da accumulatrice, la quale viene restituita
																														alla fine dell'esecuzione.
																												*)
																												let rec applyFilter lista acc =
																												(
																													match lista with
																													| [] -> acc
																													| head::tail -> (
																																						match evT_funApplication funClosure head with
																																						| Bool(true) -> applyFilter tail (head::acc)
																																						| Bool(false) -> applyFilter tail acc
																																						| _ -> failwith("The function doesn't return a bool")
																																					)
																												) in 
																													let filteredList = applyFilter lst emptyEvtList 
																														in Set(type1, List.rev filteredList)
																										| _ -> failwith("Second argument is not a set")
																									)
								| Unbound -> failwith("Function not found")
								| _ -> failwith("Runtime Error")
							)
							in set_filterFunction (eval e1 myEnv) (eval e2 myEnv)
						| Map(e1,e2) -> 
								(* set_mapFunction controlla che funClosure e set siano rispettivamente
								   una chiusura di funzione ed un set, quindi procede con l'applicare
								   la funzione ad ogni elemento del set *)
									let set_mapFunction (funClosure: evT) (set: evT): evT =
									(
										match funClosure with
										| Closure(argIde, fBody, fEnv) -> 
											(
												match set with
												| Set(type1,valuesList) -> 
													(* applico il predicato a tutti gli elementi nel set *)
													let rec aux lista = 
														(
															match lista with
															| [] -> emptyEvtList
															| h::t -> (evT_funApplication funClosure h)::(aux t)
														) in Set(type1,aux valuesList)
												| _ -> failwith("Second argument is not a set")
											)
										| Unbound -> failwith("Function not found")
										| _ -> failwith("Run-time error") 
									)
									in set_mapFunction (eval e1 myEnv) (eval e2 myEnv);;
