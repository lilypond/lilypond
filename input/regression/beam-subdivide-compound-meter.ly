\version "2.25.35"

\header {
  texidoc = "Even under unusual measure lengths, beam
subdivision should not defect."
}

\paper {
  ragged-right = ##t
}

\relative c'' {
  \omit Staff.Clef
  \timeAbbrev #'((2 4) (5 32))
  \contextPropertyCheck Timing.beatBase #1/32
  \set beatStructure = #'(8 8 5)
  \set subdivideBeams = ##t

  \*21 e32 \break

  \set beatStructure = #'(8 8 2 3)
  \*21 e32 \break
}
