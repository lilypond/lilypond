\version "2.19.21"

\header {
  texidoc = "Seconds do not confuse the collision algorithm.
The first pair of chords in each measure should merge, mesh,
or come relatively close, but the second in each measure needs
more space to make clear which notes belong to which voice."
}

\relative << {
  <a' b>2 <a b d e> <a e' f> <a e' f> <a b c> <f g a>
  \bar "||" <f g c> <g a c> <f g c d> <f g c d>
} \\ {
  <g a> <a b> <g a e'> <g a c e> <f g a> <a b c>
  <g a e'> <g a e'> <g c d> <g a b>
} >>
