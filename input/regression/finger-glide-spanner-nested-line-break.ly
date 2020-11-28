\version "2.23.0"

\header {
  texidoc = "Nested @code{FingerGlideSpanner} grobs work.  A breaking line does
not disturb the printing, the part after the break continues with the same
angle."
}

{
  \set fingeringOrientations = #'(right)
  b1\glide -1
  \once \override FingerGlideSpanner.bound-details.right.padding = 4.5
  <c'\glide-3 e'\glide-2 >1
  <d'-\glide -3 fis'-\glide -2>1
  \break
  \set fingeringOrientations = #'(left)
  <e''-3 gis''-2>
  b'' -1
  \once \set fingeringOrientations = #'(up right)
  < c''\glide -3 e''\glide -2 >1
  \once \override FingerGlideSpanner.bound-details.left.padding = 4.5
  <d''\glide -3 fis''>
  \once \set fingeringOrientations = #'(up left)
  <e''-3 gis''-2>
}
