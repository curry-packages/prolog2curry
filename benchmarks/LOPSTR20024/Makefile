# Makefile for generating and executing benchmarks

# The binaries of the various systems (adapt this to the local installations):
SWI=/usr/bin/swipl
SICS=/opt/sicstus/bin/sicstus
PAKCS=/opt/pakcs/bin/pakcs
KICS2=/opt/kics2/bin/kics2

PLNAMES    = thirdof signat numleaves permsort rev tak ackermann
SWIEXEC    = $(foreach prog, $(PLNAMES), $(addsuffix _swi, $(prog)))
SICSEXEC   = $(foreach prog, $(PLNAMES), $(addsuffix _sics, $(prog)))

CURRYNAMES = Thirdof Signat Numleaves Permsort Rev Tak Ackermann
CURRYFILES = $(foreach prog, $(CURRYNAMES), $(addsuffix .curry, $(prog)))
PAKCSEXEC  = $(foreach prog, $(CURRYNAMES), $(addsuffix _pakcs, $(prog)))
KICS2EXEC  = $(foreach prog, $(CURRYNAMES), $(addsuffix _kics2, $(prog)))

BENCHSWI   = $(foreach prog, $(SWIEXEC),   $(addprefix bench_, $(prog)))
BENCHSICS  = $(foreach prog, $(SICSEXEC),  $(addprefix bench_, $(prog)))
BENCHPAKCS = $(foreach prog, $(PAKCSEXEC), $(addprefix bench_, $(prog)))
BENCHKICS2 = $(foreach prog, $(KICS2EXEC), $(addprefix bench_, $(prog)))

# Prolog->Curry translator:
PLCURRY=../../scripts/pl2curry-failsensitive.sh

all:
	$(MAKE) $(SWIEXEC)
	$(MAKE) $(SICSEXEC)
	$(MAKE) $(PAKCSEXEC)
	$(MAKE) $(KICS2EXEC)

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

######################################################################

bench_%: %
	@echo "RUNNING BENCHMARK: $*"
	@time -p ./$*
	@time -p ./$*
	@time -p ./$*

%_swi: %.pl
	@echo Generating $@ from $<
	echo "compile($*), qsave_program('$@',[toplevel((main(R),write(R),nl,halt))])." | $(SWI) --stack_limit=4G

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
	$(KICS2) --nocypm :set dfs :l $* :save :q
	mv $* $@

Thirdof.curry: thirdof.pl
	$(PLCURRY) $<

Signat.curry: signat.pl
	$(PLCURRY) $<

Numleaves.curry: numleaves.pl
	$(PLCURRY) $<

Rev.curry: rev.pl
	$(PLCURRY) $<

Permsort.curry: permsort.pl
	$(PLCURRY) $<

Tak.curry: tak.pl
	$(PLCURRY) $<

Ackermann.curry: ackermann.pl
	$(PLCURRY) $<

.PHONY: clean
clean:
	rm -f $(SWIEXEC) $(SICSEXEC) $(PAKCSEXEC) $(KICS2EXEC)
	#rm -f $(CURRYFILES)
	rm -rf .curry
