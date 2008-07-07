
\version "2.11.51"
\header {

  texidoc = "It is possible to associate
fingerings uniquely with notes. This makes it possible to add
horizontal fingerings to notes.

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

  \once \override Fingering  #'staff-padding = #'()
  < c-1  e-2 g-3 b-5 > 4

  \set fingeringOrientations = #'(up left)
  < c-1  e-2 g-3 b-5 > 4

  \set fingeringOrientations = #'(right)
  < c-1  e-2 g-3 b-5 > 4

}





