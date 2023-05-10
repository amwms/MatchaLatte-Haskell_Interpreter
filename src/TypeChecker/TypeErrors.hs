module TypeChecker.TypeErrors where
import Grammar.Abs (BNFC'Position, Type, Ident)
import TypeChecker.CheckerUtils
import TypeChecker.CheckerTypes
import Control.Monad.Except
import Data.Functor.Contravariant (comparisonEquivalence)
import GHC.ExecutionStack (Location(functionName))

initializationTypesError :: BNFC'Position -> Type -> Type -> String
initializationTypesError pos varType exprType =
    "Initialization error - expected type " ++ showType varType ++ 
    " but got " ++ showType exprType ++ 
    " in position (" ++ showPosition pos ++ ")"

catchWrongReturnTypeError :: BNFC'Position -> Type -> Type -> TypeCheckerMonad ()
catchWrongReturnTypeError pos expectedType actualType = 
    unless (compareTypes expectedType actualType) $
        throwError $ "Return type error - expected type " ++ showType expectedType ++
        " but return value was type " ++ showType actualType ++ 
        " in position (" ++ showPosition pos ++ ")"

noReturnValueForNonVoidFunctionError :: BNFC'Position -> Type -> String
noReturnValueForNonVoidFunctionError pos retType =
    "No return value for non-void function in position" ++ showPosition pos ++
    " but expected type " ++ showType retType

returnOutsideOfFunctionError :: BNFC'Position -> String
returnOutsideOfFunctionError pos =
    "Return outside of function in position " ++ showPosition pos

mainFunctionTypeError :: BNFC'Position -> Type -> String
mainFunctionTypeError pos retType =
    "Main function must return an integer value in position " ++ showPosition pos

noMainFunctionFoundError :: String
noMainFunctionFoundError =
    "No main function found"

wrongNumberOfArgumentsInApplicationError :: BNFC'Position -> String
wrongNumberOfArgumentsInApplicationError pos =
    "Application error - wrong number of arguments in " ++ showPosition pos

catchWrongArgumentTypeError :: BNFC'Position -> Type -> Type -> TypeCheckerMonad ()
catchWrongArgumentTypeError pos expectedType actualType = 
    unless (compareTypes expectedType actualType) $
        throwError $ "Argument error - expected type " ++ showType expectedType ++
        " but got " ++ show actualType ++ " in position (" ++ showPosition pos ++ ")"

referenceAssignedNonReferenceTypeError :: BNFC'Position -> String
referenceAssignedNonReferenceTypeError pos =
    "Reference error - argument cannot be assigned a non-variable expression in position (" ++ 
    showPosition pos ++ ")"
 
boolOperationTypeError :: BNFC'Position -> String -> Type -> Type -> String
boolOperationTypeError pos operationName valType1 valType2 =
    operationName ++ " operation error in position (" ++ showPosition pos ++ ") - " ++
    showType valType1 ++ " and " ++ showType valType2 ++
    " are not both boolean values"

comparisonTypesError :: BNFC'Position -> Type -> Type -> String
comparisonTypesError pos valType1 valType2 =
    "Comparison error in position (" ++ showPosition pos ++ 
    ") - error comparing different types " ++ 
    showType valType1 ++ " and " ++ showType valType2

mathOperationTypeError :: BNFC'Position -> String -> Type -> Type -> String
mathOperationTypeError pos operationName valType1 valType2 =
    "Math operation error - cannot " ++ 
    operationName ++ " types " ++ 
    showType valType1 ++ " and " ++ showType valType2 ++ 
    " in position (" ++ showPosition pos ++ ")"

genericExpressionTypeInPositionError :: BNFC'Position -> String -> String -> Type -> String
genericExpressionTypeInPositionError pos functionName typeName valType =
   functionName ++ " error  - expression of type " ++
   showType valType ++
   " in position " ++ showPosition pos ++ 
   " is not a " ++ typeName ++  " value"

genericVariableTypeInPositionError :: BNFC'Position -> String -> Ident -> Type -> Type -> String
genericVariableTypeInPositionError pos functionName ident valType expectedType =
   functionName ++ " error  - variable " ++ show ident ++ 
   " is of type " ++ showType valType ++
   " in position " ++ showPosition pos ++ 
   " but expected type was " ++ showType expectedType