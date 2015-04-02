As an attempt to keep the semantics in Prolog syntax but still enable
a more paper-readable form I have started writing a converter from
ProLog (perhaps extended with some layout) to LaTeX.  As a side-effect
I learn about the intricate syntax (and possibly semantics) of Prolog.

The main file is labelled BNFC grammar in Prolog.cf.

TODO:
* Why are there so many similar operators: "=", "is", "==", "=..", "\\=", "\\==" ?
* Clean up the precedences and fix the pretty-printing of ";" to not break the line.
