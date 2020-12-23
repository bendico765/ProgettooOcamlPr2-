(* tipi di base del mio linguaggio *)
type ide = string;;
type types = 
	| IntType 
	| BoolType 
	| StringType
	| SetType of types
type evT =
	| Int of int
	| Bool of bool
	| String of string
	| Set of types*evT list
	| Unbound;;

(* possibili espressioni del linguaggio e loro significato *)
type exp = 
	| Den of ide
	| CstInt of int
	| CstBool of bool
	| CstString of string
	| Sum of exp*exp
	| Let of ide*exp*exp
	| EmptySet of types
	| Singleton of exp*types
	| Insert of exp*exp (* set*valore *)
	| Remove of exp*exp (* set*valore *)
	| IsEmpty of exp
	| SetMax of exp
	| SetMin of exp
	| IsIn of exp*exp (* set*valore *)
	| IsSubsetOf of exp*exp (* setContenitore*setContenuto *);;

(* Definizione del typechecker dinamico *)
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

(* Definizione dell'ambiente *)
type 't env = (ide * 't) list;;
let emptyEnv : evT env = [];;
let bind (env: evT env) (id: ide) (value: evT) = (id,value)::env;;
let rec lookup (myEnv: evT env) (x: ide) : evT= 
	match myEnv with
	| (y,v)::t -> if x = y then v else lookup t x
	| [] -> Unbound;;

(* implementazione delle funzioni legate al tipo Set *)
let emptyEvtList : evT list = [];; 
let emptySet (typeOfSet: types): evT = 
	match typeOfSet with
	| SetType(_) -> failwith("A set cannot cointain another set")
	| _ -> Set(typeOfSet,emptyEvtList);; 
let singletonSet (v: evT) (setType: types): evT = (*PERMETTE DI CREARE SET DI SET*)
	match typecheck setType v with
	| true -> Set(setType, v::emptyEvtList)
	| false -> failwith("Set type and value type don't match");;
let bool_IsEmpty (value: evT): bool = 
	match value with
	| Set(_,[]) -> true
	| Set(_,head::tail) -> false
	| _ -> failwith("Run-time error: this is not a set")
let set_setInsertion (set: evT) (value: evT): evT =
	let valueType = getType(value) in
		let expectedSetType = SetType(valueType) in
			match typecheck expectedSetType set with
			| true -> (
						match set with
						| Set(typeOfSet,[]) -> Set(typeOfSet, [value])
						| Set(typeOfSet,list) -> 
							if List.mem value list then failwith("Elemento già esistente")
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
					  )
			| false -> failwith("Value and set type don't match");;
let evT_findMax (v: evT) : evT =
	match getType(v) with
	| SetType(_) ->	match v with 
					| Set(_, []) -> failwith("The set is empty")
					| Set(_,head::tail) -> (
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
					  )
			| false -> failwith("Value and set type don't match");;
let bool_isSubsetOf (set1: evT) (set2: evT): bool = 
	match (set1,set2) with
	| (Set(type1, list1),Set(type2, list2)) -> if type1 <> type2 then failwith("Type mismatch")
								 else (
								 	let isInList1 = fun elem -> List.mem elem list1 
								 		in List.for_all isInList1 list2
								 )
	| (Set(_), _) -> failwith("Second element is not a set")
	| (_, Set(_)) -> failwith("First element is not a set")
	| (_,_) -> failwith("Both elements are not sets")
(* implementazione delle funzioni di base del linguaggio *)
let int_sum (v1: evT) (v2: evT) : evT =
	match (typecheck IntType v1, typecheck IntType v2) with
	| (true,true) -> ( match (v1,v2) with
					 | (Int(x),Int(y)) -> Int(x+y)
					 | _ -> failwith("run-time error") )
	| (_,_) -> failwith("Type error in Sum");;
	
(* Interprete *)
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
	| IsSubsetOf(e1,e2) -> Bool(bool_isSubsetOf (eval e1 myEnv) (eval e2 myEnv));;
