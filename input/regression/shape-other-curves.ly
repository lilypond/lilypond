\version "2.15.39"

\header {
  texidoc = "In addition to @code{Slur}, the music function @code{\\shape} works
with @code{PhrasingSlur}, @code{Tie}, @code{LaissezVibrerTie}, and @code{RepeatTie}.
Each is shown below, first unmodified and then (in blue) after application of the
function."
}

\layout {
  indent = 0
  ragged-right = ##t
}

\relative c'' {
  % PhrasingSlur
  d4\( d' b g g,8 f' e d c2\)
  \override PhrasingSlur #'color = #blue
  \shape PhrasingSlur #'((0 . -2) (-1 . 3.5) (0.5 . 0.5) (0 . -2.5))
  d4\( d' b g g,8 f' e d c2\)
  \break

  % Tie
  cis1~
  \break
  cis
  \override Tie #'color = #blue
  \shape Tie #'(() ((0 . -0.9) (0 . -0.5) (0 . -0.5) (0 . -0.9)))
  cis~
  \break
  cis
  \break

  % LaissezVibrerTie
  c\laissezVibrer
  \override LaissezVibrerTie #'color = #blue
  \shape LaissezVibrerTie #'((0 . 0) (0.5 . 0.2) (1.5 . 0.2) (2 . 0))
  c\laissezVibrer
  \break

  % RepeatTie
  c\repeatTie
  \override RepeatTie #'color = #blue
  \shape RepeatTie #'((-1 . 0) (-0.7 . 0) (-0.3 . 0) (0 . 0))
  c\repeatTie
}
