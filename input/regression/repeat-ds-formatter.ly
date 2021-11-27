\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="The format of @emph{D.S.} and related instructions can be
customized by overriding the @code{dalSegnoTextFormatter} context
property.  The line should end with the instruction
@emph{A@tie{}SIGNO}."
}

#(define (test-ds-formatter context return-count marks) "A SIGNO")

\new Score \with { dalSegnoTextFormatter = #test-ds-formatter } {
  R1
  \repeat segno 2 {
    R1*2
  }
}
