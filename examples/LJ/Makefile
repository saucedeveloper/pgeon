# $Id$

SHELL:=/bin/bash
LOGIC=lj.txt
USAT=tests/p0.txt tests/p1.txt tests/p2.txt tests/p3.txt tests/p4.txt tests/p5.txt tests/p6.txt
UNSATLOG=$(patsubst %.txt,%.txt.log,$(USAT))
SAT=tests/f0.txt tests/f1.txt tests/f2.txt tests/f3.txt
SATLOG=$(patsubst %.txt,%.txt.log,$(SAT))
EXEC=dune exec pgeon -- $(LOGIC)

all: unsat sat clean

%.log: %
	@timeout 1 $(EXEC) $< > $@ 2>/dev/null || echo "Timeout" > $@

unsat: $(UNSATLOG)
	@echo -e "\033[1;33m>>>> Testing success files:\033[0;39m";
	@for i in $^; do \
		echo -n "$$i: "; \
		grep Success $$i > /dev/null && echo -e "\033[32mSuccess\033[39m"; \
		grep Failure $$i > /dev/null && echo -e "\033[33mFailure\033[39m"; \
		grep Timeout $$i > /dev/null && echo -e "\033[33mTimeout\033[39m"; \
		true; \
		done

sat: $(SATLOG)
	@echo -e "\033[1;33m>>>> Testing failure files:\033[0;39m";
	@for i in $^; do \
		echo -n "$$i: "; \
		grep Success $$i > /dev/null && echo -e "\033[32mSuccess\033[39m"; \
		grep Failure $$i > /dev/null && echo -e "\033[31mFailure\033[39m"; \
		grep Timeout $$i > /dev/null && echo -e "\033[33mTimeout\033[39m"; \
		true; \
		done

clean:
	@ rm -f $(SATLOG) $(UNSATLOG)
