\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Where a @emph{D.C.} or @emph{D.S.} instruction is not
aligned on a measure boundary, the bar line defined by
@code{underlyingRepeatBarType} appears by default.  In this case, the
@emph{D.C.} should have a normal bar line and the @emph{D.S.} should
have a dotted bar line."
}

\new Score \with { underlyingRepeatBarType = ";" } {
  \fixed c' {
    \partial 4
    \repeat segno 2 {
      f4 | f2.
    }
    \bar "|" % override underlyingRepeatBarType
    \repeat segno 2 {
      g4 | g2.
    }
  }
}
