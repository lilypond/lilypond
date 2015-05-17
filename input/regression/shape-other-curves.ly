\version "2.19.21"

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

\relative {
  % PhrasingSlur
  d''4\( d' b g g,8 f' e d c2\)
  \override PhrasingSlur.color = #blue
  \shape #'((0 . -2) (-1 . 3.5) (0.5 . 0.5) (0 . -2.5)) PhrasingSlur
  d4\( d' b g g,8 f' e d c2\)
  \break

  % Tie
  cis1~
  \break
  cis
  \override Tie.color = #blue
  \shape #'(() ((0 . -0.9) (0 . -0.5) (0 . -0.5) (0 . -0.9))) Tie
  cis~
  \break
  cis
  \break

  % LaissezVibrerTie
  c\laissezVibrer
  \override LaissezVibrerTie.color = #blue
  \shape #'((0 . 0) (0.5 . 0.2) (1.5 . 0.2) (2 . 0)) LaissezVibrerTie
  c\laissezVibrer
  \break

  % RepeatTie
  c\repeatTie
  \override RepeatTie.color = #blue
  \shape #'((-1 . 0) (-0.7 . 0) (-0.3 . 0) (0 . 0)) RepeatTie
  c\repeatTie
}
