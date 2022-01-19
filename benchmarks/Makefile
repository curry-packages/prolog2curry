# Makefile for generating and executing benchmarks

# The binaries of the various systems:
SWI=swipl
SICS=/opt/sicstus/bin/sicstus
PAKCS=pakcs3
KICS2=/opt/kics2/kics3/bin/kics2
CURRY2GO=curry2go
GHC=ghc

PLNAMES    = rev takint takpeano ackermann
SWIEXEC    = $(foreach prog, $(PLNAMES), $(addsuffix _swi, $(prog)))
SICSEXEC   = $(foreach prog, $(PLNAMES), $(addsuffix _sics, $(prog)))

CURRYNAMES = Rev Takint Takpeano Ackermann
CURRYFILES = $(foreach prog, $(CURRYNAMES), $(addsuffix .curry, $(prog)))
PAKCSEXEC  = $(foreach prog, $(CURRYNAMES), $(addsuffix _pakcs, $(prog)))
KICS2EXEC  = $(foreach prog, $(CURRYNAMES), $(addsuffix _kics2, $(prog)))
C2GEXEC    = $(foreach prog, $(CURRYNAMES), $(addsuffix _c2g,   $(prog)))
HSFILES    = $(foreach prog, $(CURRYNAMES), $(addsuffix .hs,    $(prog)))
GHCEXEC    = $(foreach prog, $(CURRYNAMES), $(addsuffix _ghc,   $(prog)))
GHCHI      = $(foreach prog, $(CURRYNAMES), $(addsuffix .hi,    $(prog)))
GHCO       = $(foreach prog, $(CURRYNAMES), $(addsuffix .o,     $(prog)))

BENCHSWI   = $(foreach prog, $(SWIEXEC),   $(addprefix bench_, $(prog)))
BENCHSICS  = $(foreach prog, $(SICSEXEC),  $(addprefix bench_, $(prog)))
BENCHPAKCS = $(foreach prog, $(PAKCSEXEC), $(addprefix bench_, $(prog)))
BENCHKICS2 = $(foreach prog, $(KICS2EXEC), $(addprefix bench_, $(prog)))
BENCHC2G   = $(foreach prog, $(C2GEXEC),   $(addprefix bench_, $(prog)))
BENCHGHC   = $(foreach prog, $(GHCEXEC),   $(addprefix bench_, $(prog)))

all:
	$(MAKE) $(SWIEXEC)
	$(MAKE) $(SICSEXEC)
	$(MAKE) $(PAKCSEXEC)
	$(MAKE) $(KICS2EXEC)
	$(MAKE) $(GHCEXEC)

# Running all Prolog benchmarks with SWI-Prolog:
.PHONY: benchswi
benchswi:
	$(MAKE) $(SWIEXEC)
	$(MAKE) $(BENCHSWI)

# Running all Prolog benchmarks with SICStus-Prolog:
.PHONY: benchsics
benchsics:
	$(MAKE) $(SICSEXEC)
	$(MAKE) $(BENCHSICS)

# Running all Curry benchmarks with PAKCS:
.PHONY: benchpakcs
benchpakcs:
	$(MAKE) $(PAKCSEXEC)
	$(MAKE) $(BENCHPAKCS)

# Running all Curry benchmarks with KICS2:
.PHONY: benchkics2
benchkics2:
	$(MAKE) $(KICS2EXEC)
	$(MAKE) $(BENCHKICS2)

# Running all Curry benchmarks with Curry2Go:
.PHONY: benchc2g
benchc2g:
	$(MAKE) $(C2GEXEC)
	$(MAKE) $(BENCHC2G)

# Running all Haskell benchmarks with GHC:
.PHONY: benchghc
benchghc:
	$(MAKE) $(GHCEXEC)
	$(MAKE) $(BENCHGHC)

######################################################################

bench_%: %
	@echo "RUNNING BENCHMARK: $*"
	@time -p ./$*
	@time -p ./$*
	@time -p ./$*
	@time -p ./$*

%_swi: %.pl
	@echo Generating $@ from $<
	echo "compile($*), qsave_program('$@',[toplevel((main(R),write(R),nl,halt))])." | $(SWI)

%_sics: %.pl
	@echo Generating $@ from $<
	echo "compile($*), save_program('$@',(main(R),write(R),nl,halt))." | $(SICS)
	mv $@.sav $@
	chmod 755 $@

%_pakcs: %.curry
	@echo Generating $@ from $<
	$(PAKCS) --nocypm :l $* :save :q
	mv $* $@

%_kics2: %.curry
	@echo Generating $@ from $<
	$(KICS2) --nocypm :l $* :save :q
	mv $* $@

%_c2g: %.curry
	@echo Generating $@ from $<
	$(CURRY2GO) --nocypm :l $* :save :q
	mv $* $@

Rev.curry: rev.pl
	pl2curry $<

Takint.curry: takint.pl
	pl2curry $<

Takpeano.curry: takpeano.pl
	pl2curry $<

Ackermann.curry: ackermann.pl
	pl2curry $<

%.hs: %.curry
	cat $< | tail -n +4 | sed -e 's/^main =/main = print (/' | sed -e 's/^main =.*/& )/' > $@

%_ghc: %.hs
	@echo Generating $@ from $<
	$(GHC) -O2 --make $*
	mv $* $@

.PHONY: clean
clean:
	rm -f $(SWIEXEC) $(SICSEXEC) $(PAKCSEXEC) $(KICS2EXEC) $(C2GEXEC)
	rm -f $(CURRYFILES) $(HSFILES) $(GHCEXEC) $(GHCHI) $(GHCO)
	rm -rf .curry

