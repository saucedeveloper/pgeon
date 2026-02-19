# $Id$

SHELL:=/bin/bash
LOGIC=lk.txt
TESTS=tests
SUCCESS=p0.txt p1.txt p2.txt p3.txt p4.txt p5.txt p6.txt
FAILURE=f0.txt f1.txt f2.txt
EXEC=eval $$(opam env); dune exec pgeon -- $(LOGIC)

all: success failure

success:
	-@echo -e "\033[1;33m>>>> Testing success files:\033[0;39m"; \
        for i in $(SUCCESS); do echo -n "Testing $$i: "; \
        $(EXEC) $(TESTS)/$$i > tmp.res 2>&1; \
        res=`grep "Success" tmp.res`; rm tmp.res; \
        echo -e "\033[32m$$res\033[39m"; done

failure:
	-@echo -e "\033[1;33m>>>> Testing failure files:\033[0;39m"; \
        for i in $(FAILURE); do echo -n "Testing $$i: "; \
        $(EXEC) $(TESTS)/$$i > tmp.res 2>&1; \
        res=`grep "Failure" tmp.res`; rm tmp.res; \
        echo -e "\033[31m$$res\033[39m"; done

clean:
	rm *~ $(TESTS)/*~
