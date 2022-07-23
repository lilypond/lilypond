\version "2.23.12"

\header {
  texidoc = "Let @code{ly:context-output-def} access some output
variables from inside a @code{\\applyContext} expression."
}

#(use-modules (lily display-lily))

showvar =
#(define-music-function (sym) (symbol?)
   (make-apply-context
    (lambda (c)
      (format #t "\n~s: ~a" sym
              (value->lily-string (ly:output-def-lookup
                                   (ly:context-output-def c) sym))))))

music =
{
  \showvar output-def-kind
  \showvar flag
  c'1
}


\score {
  \music
  \layout {
    flag = 0
  }
  \midi {
    flag = 1
  }
}

\score {
  \music
  \layout {
    flag = 2
  }
}
