\version "2.25.22"

\header {
  texidoc = "
Tuplets that span more than one beat should be
subdivided if subdivideBeams is #t.  In this example,
the beams should be subdivided every 1/8.
"

}

\relative c' {
    \tupletSpan 4
    \set beatBase = #1/8
    \set subdivideBeams = ##t
    \tuplet 6/4 { \repeat unfold 24 { c16 } }
}
