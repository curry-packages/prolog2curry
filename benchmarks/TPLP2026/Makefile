# Makefile for generating and executing benchmarks

# The binaries of the various systems:
SWI=/usr/bin/swipl
SICS=/opt/sicstus/bin/sicstus
PAKCS=/opt/pakcs/bin/pakcs
KICS2=/opt/kics2/bin/kics2
KMCC=$(HOME)/kmcc/bin/kmcc

PROGNAMES  = Rev Takpeano Takint Ackermann Primes Quicksort Permsort Queens Sendmore Zebra Thirdof Signat Numleaves Sublist Pali ExprSyntax
#PROGNAMES  = Takpeano_strict # only for Curry benchmark

SWIEXEC    = $(foreach prog, $(PROGNAMES), $(addsuffix _swi, $(prog)))
SICSEXEC   = $(foreach prog, $(PROGNAMES), $(addsuffix _sics, $(prog)))
CURRYFILES = $(foreach prog, $(PROGNAMES), $(addsuffix .curry, $(prog)))
PAKCSEXEC  = $(foreach prog, $(PROGNAMES), $(addsuffix _pakcs, $(prog)))
KICS2EXEC  = $(foreach prog, $(PROGNAMES), $(addsuffix _kics2, $(prog)))
KMCCEXEC   = $(foreach prog, $(PROGNAMES), $(addsuffix _kmcc,  $(prog)))

BENCHSWI   = $(foreach prog, $(SWIEXEC),   $(addprefix bench_, $(prog)))
BENCHSICS  = $(foreach prog, $(SICSEXEC),  $(addprefix bench_, $(prog)))
BENCHPAKCS = $(foreach prog, $(PAKCSEXEC), $(addprefix bench_, $(prog)))
BENCHKICS2 = $(foreach prog, $(KICS2EXEC), $(addprefix bench_, $(prog)))
BENCHKMCC  = $(foreach prog, $(KMCCEXEC),  $(addprefix bench_, $(prog)))

# Prolog->Curry translator:
PLCURRY=../../scripts/pl2curry-failsensitive.sh

.PHONY: all
all:
	$(MAKE) $(SWIEXEC)
	$(MAKE) $(SICSEXEC)
	$(MAKE) $(PAKCSEXEC)
	$(MAKE) $(KICS2EXEC)
	$(MAKE) $(KMCCEXEC)

# Translate all Prolog programs into Curry programs:
.PHONY: curryfiles
curryfiles:
	$(MAKE) $(CURRYFILES)

# Running all benchmarks:
.PHONY: benchall
benchall:
	$(MAKE) benchswi
	$(MAKE) benchsics
	$(MAKE) benchpakcs
	$(MAKE) benchkics2
	$(MAKE) benchkmcc

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

# Running all Curry benchmarks with KMCC:
.PHONY: benchkics2
benchkmcc:
	$(MAKE) $(KMCCEXEC)
	$(MAKE) $(BENCHKMCC)

######################################################################

# command for timing:
TIMECMD=/usr/bin/time -f "ELAPSED TIME (seconds): %e\nMAXIMUM RESIDENT SET SIZE (kbytes): %M"
bench_%: %
	@echo "RUNNING BENCHMARK: $*"
	@$(TIMECMD) ./$*
#stop:
	@sleep 1
	@$(TIMECMD) ./$*
	@sleep 1
	@$(TIMECMD) ./$*
	@sleep 1

%_swi: %.pl
	@echo Generating $@ from $<
	echo "compile('$*'), qsave_program('$@',[toplevel((main(R),write(R),nl,fail;halt))])." | $(SWI) --stack_limit=4G

%_sics: %.pl
	@echo Generating $@ from $<
	echo "compile('$*'), save_program('$@',(main(R),write(R),nl,fail;halt))." | $(SICS)
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

%_kmcc: %.curry
	@echo Generating $@ from $<
	$(KMCC) --nocypm :set dfs :l $* :save :q
	mv $* $@

# Transform a Prolog program into a Curry program with the
# fail-sensitive demand functional transformation:
%.curry: %.pl
	$(PLCURRY) --time $<

.PHONY: clean
clean:
	rm -f $(SWIEXEC) $(SICSEXEC) $(PAKCSEXEC) $(KICS2EXEC) $(KMCCEXEC)
	#rm -f $(CURRYFILES) # remove also generated Curry files
	rm -rf .curry
