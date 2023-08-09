\version "2.25.8"

\header {
  doctitle = "Beam subdivision with 2 layers of tuplets"

  texidoc = "Stems at boundaries of tuplet spans must have one side
subdivided from a perspective outside of said tuplet span"
}


\paper {
  indent = 0
  ragged-right = ##t
}

\relative c' {
  \time 1/4
  \set subdivideBeams = ##t
  \omit Staff.Clef


  c16
  \tuplet 3/2 {
    c32 c
    \tuplet 5/4 {
      \repeat unfold 5 c128
    }
  }
  \tuplet 6/4 {
    \tuplet 10/8 {
      \repeat unfold 10 c256
    }
    c64 c c c
  }
  c16
  \break
}
