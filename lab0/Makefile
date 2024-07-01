RM = rm -rf
RET = cd ..

GREEN = \033[0;32m
NC = \033[0m

.PHONY: all runall cleanall \
		c runc cleanc \
		ada runada cleanada \
		java runjava cleanjava \
		go rungo cleango \
		haskell runhaskell cleanhaskell \
		lisp runlisp cleanlisp \
		sml runsml cleansml \
		prolog runprolog cleanprolog

all: c ada java go haskell lisp sml prolog

runall: runc runada runjava rungo runhaskell runlisp runsml runprolog

cleanall: cleanc cleanada cleanjava cleango cleanhaskell cleanlisp cleansml cleanprolog

c:
	@echo "$(GREEN)Building: $(NC)C"
	@cd C && gcc -o hello_world hello_world.c && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

runc: c
	@echo "$(GREEN)Running: $(NC)C"
	@cd C && ./hello_world && $(RET)
	@echo

cleanc:
	@echo "$(GREEN)Cleaning: $(NC)C"
	@cd C && $(RM) hello_world && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

ada:
	@echo "$(GREEN)Building: $(NC)Ada"
	@cd Ada && gnatmake hello_world.adb && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

runada: ada
	@echo "$(GREEN)Running: $(NC)Ada"
	@cd Ada && ./hello_world && $(RET)
	@echo

cleanada:
	@echo "$(GREEN)Cleaning: $(NC)Ada"
	@cd Ada && gnatclean hello_world && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

java:
	@echo "$(GREEN)Building: $(NC)Java"
	@cd Java && javac HelloWorld.java && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

runjava: java
	@echo "$(GREEN)Running: $(NC)Java"
	@cd Java && java HelloWorld && $(RET)
	@echo

cleanjava:
	@echo "$(GREEN)Cleaning: $(NC)Java"
	@cd Java && $(RM) HelloWorld.class && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

go:
	@echo "$(GREEN)Building: $(NC)GoLang"
	@cd GoLang && go build -o hello_world hello_world.go && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

rungo:
	@echo "$(GREEN)Running: $(NC)GoLang"
	@cd GoLang && go run hello_world.go && $(RET)
	@echo

cleango:
	@echo "$(GREEN)Cleaning: $(NC)GoLang"
	@cd GoLang && $(RM) hello_world && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

haskell:
	@echo "$(GREEN)Building: $(NC)Haskell"
	@cd Haskell && ghc hello_world.hs && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

runhaskell: haskell
	@echo "$(GREEN)Running: $(NC)Haskell"
	@cd Haskell && ./hello_world && $(RET)
	@echo

cleanhaskell:
	@echo "$(GREEN)Cleaning: $(NC)Haskell"
	@cd Haskell && $(RM) hello_world hello_world.hi hello_world.o && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

lisp:
	@echo "$(GREEN)Building: $(NC)Lisp"
	@cd Lisp && clisp -c hello_world.lisp && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo
# run compiled file with: clisp -x '(load "hello_world.fas")'

runlisp:
	@echo "$(GREEN)Running: $(NC)Lisp"
	@cd Lisp && clisp hello_world.lisp && $(RET)
	@echo

cleanlisp:
	@echo "$(GREEN)Cleaning: $(NC)Lisp"
	@cd Lisp && $(RM) hello_world.fas hello_world.lib && $(RET)
	@echo "$(GREEN)Done$(NC)"
	@echo

sml:
	@echo "$(GREEN)Building: $(NC)SML"
	@echo Cannot build an SML program using the smlnj interpreter
	@echo

runsml:
	@echo "$(GREEN)Running: $(NC)SML"
	@cd SML && sml -m < hello_world.sml && $(RET)
	@echo

cleansml:
	@echo "$(GREEN)Cleaning: $(NC)SML"
	@echo Nothing to clean
	@echo

prolog:
	@echo "$(GREEN)Building: $(NC)Prolog"
	@echo Cannot build a Prolog program using the swipl interpreter
	@echo

runprolog:
	@echo "$(GREEN)Running: $(NC)Prolog"
	@cd Prolog && ./run.sh hello_world.pl hello_world && $(RET)
	@echo

cleanprolog:
	@echo "$(GREEN)Cleaning: $(NC)Prolog"
	@echo Nothing to clean
	@echo
