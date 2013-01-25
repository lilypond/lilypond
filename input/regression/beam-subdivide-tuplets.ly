\version "2.17.11"

\header {
  
  doctitle = "Beam subdivide tuplets"

  texidoc = "
Tuplets that span more than one beat should be
subdivided if subdivideBeams is #t.  In this example,
the beams should be subdivided every 1/8.
"

}

\relative c' {
    \tupletSpan 4
    \set baseMoment = #(ly:make-moment 1/8)
    \set subdivideBeams = ##t
    \tuplet 6/4 { \repeat unfold 24 { c16 } }
}
