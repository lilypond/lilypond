\version "2.27.2"

\header {
  texidoc = "@code{\\measure} can create a longer-than-expected measure at the
start of a piece.  The first measure should be beamed @code{2,4,2}."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    \time 2,4,2 5/8
  }
}

%% create contexts implicitly
{
  \measure { c'8 8 8 8 8 8 8 8 }
  \repeat unfold 5 d'8
  \repeat unfold 5 e'8
}
