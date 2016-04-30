# GHC = ghc-7.6.3
GHC = ghc

../../paper/semantics.tex: ../semantics.eprover
	runhaskell LaTeX.hs $< > $@

testEprover: compileEprover
	Eprover/Test ../semantics.eprover > parsed_semantics

compileEprover:
	bnfc -d Eprover.cf
	happy -gca Eprover/Par.y
	alex -g Eprover/Lex.x
#	(cd Eprover/; latex Doc.tex; dvips Doc.dvi -o Doc.ps)
	${GHC} --make Eprover/Test.hs -o Eprover/Test
cleanEprover:
	-rm -f Eprover/*.log Eprover/*.aux Eprover/*.hi Eprover/*.o Eprover/*.dvi
	-rm -f Eprover/Doc.ps
distcleanEprover: cleanEprover
	-rm -f Eprover/Doc.* Eprover/Lex.* Eprover/Par.* Eprover/Layout.* Eprover/Skel.* Eprover/Print.* Eprover/Test.* Eprover/Abs.* Eprover/Test Eprover/ErrM.* Eprover/SharedString.* Eprover/Eprover.dtd Eprover/XML.*
	-rmdir -p Eprover/

#####

testProlog: compileProlog
#	Prolog/Test ../semantics.pl > parsed_semantics
	Prolog/Test ../semantics.fpl > parsed_semantics

compileProlog:
	bnfc -d Prolog.cf
	happy -gca Prolog/Par.y
	alex -g Prolog/Lex.x
	(cd Prolog/; latex Doc.tex; dvips Doc.dvi -o Doc.ps)
	${GHC} --make Prolog/Test.hs -o Prolog/Test
cleanProlog:
	-rm -f Prolog/*.log Prolog/*.aux Prolog/*.hi Prolog/*.o Prolog/*.dvi
	-rm -f Prolog/Doc.ps
distcleanProlog: cleanProlog
	-rm -f Prolog/Doc.* Prolog/Lex.* Prolog/Par.* Prolog/Layout.* Prolog/Skel.* Prolog/Print.* Prolog/Test.* Prolog/Abs.* Prolog/Test Prolog/ErrM.* Prolog/SharedString.* Prolog/Prolog.dtd Prolog/XML.*
	-rmdir -p Prolog/

#####

clean: cleanProlog cleanEprover

distclean: distcleanProlog distcleanEprover
