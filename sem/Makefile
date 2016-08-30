%.proof: %.goal
	cat semantics.eprover $< > example.eprover
	eprover --proof-object example.eprover > $@
	more $@

example:
	cat semantics.eprover goal.eprover > example.eprover
	eprover --proof-object example.eprover

prove:
	eprover semantics.eprover

runyap:
	yap -l test.pl

rungprolog:
	gprolog --consult-file semantics.pl --consult-file test.pl

clean:
	-rm tests/*.proof

#  [ rinst(i:r1,nil,[],rte_enter(x,Cont1)), excl(i:z,free), rinst(i:r2,nil,[],rte_enter(x,Cont2)), excl(i:x,free) ] ---L---> R.

# Try as input to the REPL: example(L,R). Then step through with ";" a few times to get all the solutions. Exit with Ctrl-d.

# Alternative:
