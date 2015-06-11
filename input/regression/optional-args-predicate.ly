\version "2.19.22"

\header{
  texidoc= "Test predicate-based optional music function argument skipping."
}

\layout { ragged-right = ##t }

test=#(define-scheme-function (str int frac exp)
       ((string? "def1") (integer? "def2") (number-pair? "def3") list?)
       (if (not (equal? (list str int frac) exp))
	(ly:parser-error
	 (format #f "Expected ~a, got ~a.\n" exp
	  (list str int frac))
	 (*location*))))

\test "a" #3 #'(3 . 4) #'("a" 3 (3 . 4))
\test "a" #3 #'("a" 3 "def3")
\test "a" #'("a" "def2" "def3")
\test #'("def1" "def2" "def3")
