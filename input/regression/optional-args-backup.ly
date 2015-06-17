\version "2.19.22"

\header{
  texidoc= "Test backup of predicate-based optional music function arguments.

Unit expressions like @code{3\cm} can't be parsed as optional
arguments in one go since they would require lookahead after @code{3}.
The predicate is checked after @code{3}, and if it is suitable,
Lilypond commits to parsing as a unit number, and checks the result
again.  For the predicate @code{integer?} and @code{3\cm}, you would
actually get a syntax error (since the combination is no longer an
integer) rather than Lilypond trying to see @code{3\cm} as two
separate arguments."

}

\layout { ragged-right = ##t }

test=#(define-void-function (expect . rest)
       (list? (string? "def1") (integer? "def2") (fraction? "def3") integer?)
       (if (not (equal? expect rest))
	(ly:parser-error
	 (format #f "Expected ~s, got ~s.\n" expect rest)
	 (*location*))))

twice=2

\test #'("x" 3 (3 . 4) 8)        x 3 3/4 4\twice
\test #'("x" 3 "def3" 8)         x 3 4\twice
\test #'("x" 8 "def3" 10)        x 4\twice 5\twice
\test #'("def1" "def2" "def3" 8) 4\twice
