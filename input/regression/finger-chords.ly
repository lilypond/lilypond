
\version "2.17.6"
\header {

  texidoc = "It is possible to associate
fingerings uniquely with notes. This makes it possible to add
horizontal fingerings to notes. Fingering clears stems and flags
if @code{'add-stem-support} is set.
"

}
\layout { ragged-right= ##t }



\relative c'{
  \set fingeringOrientations = #'(left)
  < c-1  e-2 g-3 b-5 > 4

  \set fingeringOrientations = #'(down left)
  < c-1  e-2 g-3 b-5 > 4

  \set fingeringOrientations = #'(down left up)
  < c-1  e-2 g-3 b-5 > 4

  \once \override Fingering.staff-padding = #'()
  < c-1  e-2 g-3 b-5 > 4

  \set fingeringOrientations = #'(up right)
  < c-1  e-2 g-3 b-5 > 4.

  \set fingeringOrientations = #'(right)
  < c-1  e-2 g-3 b-5 > 8

  \override Fingering.add-stem-support = ##t
  \set fingeringOrientations = #'(up right)
  < c-1  e-2 g-3 b-5 > 4.

  \set fingeringOrientations = #'(right)
  < c-1  e-2 g-3 b-5 > 8

}





