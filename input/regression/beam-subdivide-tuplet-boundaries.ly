\version "2.25.35"

\header {
  texidoc = "Stems at boundaries of tuplet spans must have one side
subdivided from a perspective outside of said tuplet span."
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
      \*5 c128
    }
  }
  \tuplet 6/4 {
    \tuplet 10/8 {
      \*10 c256
    }
    c64 c c c
  }
  c16
  \break
}
