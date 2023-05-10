GHC = ghc

TYPE_CHECKER = src/TypeChecker/CheckerTypes.hs src/TypeChecker/TypeErrors.hs src/TypeChecker/CheckerUtils.hs src/TypeChecker/GrammarChecker.hs src/TypeChecker/TypeChecker.hs
GRAMMAR_FILES = src/Grammar/Abs.hs src/Grammar/Lex.hs src/Grammar/Par.hs
INTERPRETER_FILES = src/EvalExec.hs src/Evaluator.hs src/Interpreter.hs src/Utils.hs src/Types.hs src/Expr.hs src/Errors.hs

.PHONY : all clean

all : interpreter

interpreter : 
	${GHC} ${GHC_OPTS} ${GRAMMAR_FILES} ${TYPE_CHECKER} ${INTERPRETER_FILES} src/Main.hs -o interpreter
	rm -f src/*.hi src/*.o
	rm -f src/TypeChecker/*.hi src/TypeChecker/*.o
	rm -f src/Grammar/*.hi src/Grammar/*.o

clean :
	-rm -f interpreter

