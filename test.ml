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

(* METODO Union *)
let firstSet = Insert(
				Insert(
					EmptySet(IntType),
					CstInt(2)
				),
				CstInt(1)			
			 );;
let secondSet = Insert(
					Insert(EmptySet(IntType),
					CstInt(3)
				),
				CstInt(4)			
			 );;
let integerUnion = Union(firstSet,secondSet);;
eval integerUnion emptyEnv;;
let thirdSet = Insert(
					Insert(EmptySet(IntType),
					CstInt(3)
				),
				CstInt(2)			
			 );;
let duplicateUnion = Union(firstSet,thirdSet);;
eval duplicateUnion emptyEnv;;
let emptySet = EmptySet(IntType);;
let unionEmpty = Union(firstSet,emptySet);;
eval unionEmpty emptyEnv;;

(* METODO Intersect *)
let firstSet = Insert(
		Insert(
			EmptySet(IntType),
			CstInt(2)
		),
		CstInt(1)			
		);;
let secondSet = Insert(
			Insert(
				EmptySet(IntType),
				CstInt(3)
			),
			CstInt(4)			
		);;
let integerIntersect = Intersect(firstSet,secondSet);;
eval integerIntersect emptyEnv;;
let thirdSet = Insert(
		Insert(
			EmptySet(IntType),
			CstInt(3)
		),
		CstInt(2)			
		);;
let oneElementIntersect = Intersect(firstSet,thirdSet);;
eval oneElementIntersect emptyEnv;;
let emptySet = EmptySet(IntType);;
let intersectEmpty = Intersect(firstSet,emptySet);;
eval intersectEmpty emptyEnv;;

(* METODO Difference*)
let firstSet = Insert(Insert(Insert(EmptySet(IntType),CstInt(3)),CstInt(2)), CstInt(1));;
let secondSet = Insert(Insert(Insert(EmptySet(IntType),CstInt(4)),CstInt(3)), CstInt(2));;
let integerSetsDifference = Difference(firstSet,secondSet);;
eval integerSetsDifference emptyEnv;;
let integerSetsDifference = Difference(secondSet,firstSet);;
eval integerSetsDifference emptyEnv;;
let emptySetsDifference1 = Difference(EmptySet(IntType),firstSet);;
eval emptySetsDifference1 emptyEnv;;
let emptySetsDifference2 = Difference(firstSet,EmptySet(IntType));;
eval emptySetsDifference2 emptyEnv;;
let emptySetsDifference3 = Difference(EmptySet(IntType),EmptySet(IntType));;
eval emptySetsDifference3 emptyEnv;;
let emptySetDifference = Difference(firstSet,firstSet);;
eval emptySetDifference emptyEnv;;

(* METODO Map *)
let integerSet = Insert(Insert(Insert(EmptySet(IntType),CstInt(3)),CstInt(2)), CstInt(1));;
let incrementFunction = Let("incFun", Fun("x", Sum(Den("x"), CstInt(1))), Map("incFun", integerSet));;
eval incrementFunction emptyEnv;;

let equal2Function = Let("stupidFun", Fun("x", Equal(Den("x"), CstInt(2))), Map("stupidFun", integerSet));;
eval equal2Function emptyEnv;;

let incrementEmptySet = Let("incFun", Fun("x", Sum(Den("x"), CstInt(1))), Map("incFun", EmptySet(BoolType)));;
eval incrementEmptySet emptyEnv;;

let incrementErrorSet = Let("incFun", Fun("x", Sum(Den("x"), CstInt(1))), Map("incFun", Singleton(CstBool(true), BoolType)));;
eval incrementErrorSet emptyEnv;;

