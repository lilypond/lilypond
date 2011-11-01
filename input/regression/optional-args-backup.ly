\version "2.15.17"

\header{
  texidoc= "Test backup of predicate-based optional music function arguments.

Unit expressions like @code{3\cm} can't be parsed as optional
arguments since they would require lookahead after @code{3}.  However,
if @code{3} gets rejected as an optional argument based on evaluating
its predicate, it should still be able to participate as part of a unit
expression in a following mandatory argument."
}

\layout { ragged-right = ##t }

test=#(define-void-function (parser location expect . rest)
       (list? (string? "def1") (integer? "def2") (number-pair? "def3") integer?)
       (if (not (equal? expect rest))
	(ly:parser-error parser
	 (format #f "Expected ~a, got ~a.\n" expect rest)
	 location)))

twice=2

\test #'("a" 3 (3 . 4) 8)        "a" 3 3/4 4\twice
\test #'("a" 3 "def3" 8)         "a" 3 4\twice
\test #'("a" 4 "def3" 2)         "a" 4\twice
\test #'("def1" "def2" "def3" 8) 4\twice
