\version "2.19.12"

\header {
  texidoc = "@code{add-stem-support} can be removed or implemented
only for beamed notes.
"
}

music = {
  \clef bass
  \stemUp
  <g^3 a^5>2..->
  r16 eeses'16
  \set fingeringOrientations = #'(right)
  <c e>8-1-4 <c^1 e^4> <g,-3 b,-4> r
  r2
}

{
  \music
  \override Fingering.add-stem-support = ##f
  \music
  \override Fingering.add-stem-support = #only-if-beamed
  \music
}