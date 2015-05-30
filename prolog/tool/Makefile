GHC = ghc-7.6.3

test: compile
	Prolog/Test ../semantics.pl > parsed_semantics

compile:
	bnfc -d Prolog.cf
	happy -gca Prolog/Par.y
	alex -g Prolog/Lex.x
	(cd Prolog/; latex Doc.tex; dvips Doc.dvi -o Doc.ps)
	${GHC} --make Prolog/Test.hs -o Prolog/Test
clean:
	-rm -f Prolog/*.log Prolog/*.aux Prolog/*.hi Prolog/*.o Prolog/*.dvi
	-rm -f Prolog/Doc.ps
distclean: clean
	-rm -f Prolog/Doc.* Prolog/Lex.* Prolog/Par.* Prolog/Layout.* Prolog/Skel.* Prolog/Print.* Prolog/Test.* Prolog/Abs.* Prolog/Test Prolog/ErrM.* Prolog/SharedString.* Prolog/Prolog.dtd Prolog/XML.*
	-rmdir -p Prolog/
