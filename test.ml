(* TEST *)
Sys.command "clear";;

(* METODO IsEmpty *)
let integerSetExp = Let("integerSet", Singleton(CstInt(3),IntType), IsEmpty(Den("integerSet")));;
eval integerSetExp emptyEnv;;
let emptySetExp = Let("emptyIntegerSet", EmptySet(IntType), IsEmpty(Den("emptyIntegerSet")));;
eval emptySetExp emptyEnv;;
let emptySetErrorExp = Let("notASet", CstInt(3), IsEmpty(Den("notASet")));;
eval emptySetErrorExp emptyEnv;;

(*METODO Insert*)
let emptySetExp = Let("emptyIntegerSet", EmptySet(IntType), 
					Insert(
						Insert(
							Den("emptyIntegerSet"), 
							CstInt(3)
						),
						CstInt(2)
					)
			 	);;
eval emptySetExp emptyEnv;;
let duplicateElementErrorExp = Let("duplicateSet", Singleton(CstInt(3),IntType),Insert(Den("duplicateSet"), CstInt(3)));;
eval duplicateElementErrorExp emptyEnv;; 
let mismatchTypeErrorExp = Let("mismatchTypeSet", Singleton(CstInt(3),IntType),Insert(Den("mismatchTypeSet"),CstBool(true)));;
eval mismatchTypeErrorExp emptyEnv;;

(*METODO Remove*)
let removeIntegerExp = Remove(Singleton(CstInt(3),IntType), CstInt(3));;
eval removeIntegerExp emptyEnv;;
let errorRemoveEmptyExp = Remove(EmptySet(IntType), CstInt(3));;
eval errorRemoveEmptyExp emptyEnv;;
let errorMismatchExp = Remove(Singleton(CstInt(2),IntType), CstBool(true));;
eval errorMismatchExp emptyEnv;;
let removeIntegerExp = Remove(Singleton(CstInt(2),IntType), CstInt(3));;
eval removeIntegerExp emptyEnv;;

(*METODO SetMax e SetMin*)
let integerBigSet = Insert(
				Insert(
					Insert(
						Insert(
							EmptySet(IntType), CstInt(4)
						), 
						CstInt(3)
					), 
					CstInt(2)
				),
				CstInt(1)			
			 );;
let maxExp = SetMax(integerBigSet);;
eval maxExp emptyEnv;;
let minExp = SetMin(integerBigSet);;
eval minExp emptyEnv;;

let stringBigSet = Insert(
				Insert(
					Insert(
						Insert(
							EmptySet(StringType), CstString("a")
						), 
						CstString("b")
					), 
					CstString("c")
				),
				CstString("d")			
			 );;
let maxExp = SetMax(stringBigSet);;
eval maxExp emptyEnv;;
let minExp = SetMin(stringBigSet);;
eval minExp emptyEnv;;

(*METODO IsIn*)
let mismatchErrorExp = IsIn(Singleton(CstInt(3),IntType),CstBool(true));;
eval mismatchErrorExp emptyEnv;;
let successResearchExp = IsIn(Singleton(CstInt(3), IntType),CstInt(3));;
eval successResearchExp emptyEnv;;
let failedResearchExp = IsIn(Singleton(CstInt(3), IntType),CstInt(4));;
eval failedResearchExp emptyEnv;;
let emptySetResearchExp = IsIn(EmptySet(IntType),CstInt(4));;
eval emptySetResearchExp emptyEnv;;

(*METODO IsSubsetOf *)
let firstSet = Insert(
				Insert(
					Insert(
						Insert(
							EmptySet(IntType), CstInt(4)
						), 
						CstInt(3)
					), 
					CstInt(2)
				),
				CstInt(1)			
			 );;
let secondSet = Insert(
					Insert(EmptySet(IntType),
					CstInt(2)
				),
				CstInt(1)			
			 );;
let successSubset = IsSubsetOf(firstSet, secondSet);;
eval successSubset emptyEnv;;
let failSubset = IsSubsetOf(secondSet, firstSet);;
eval failSubset emptyEnv;;
let emptySubset = IsSubsetOf(firstSet, EmptySet(IntType));;
eval emptySubset emptyEnv;;
