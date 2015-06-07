\version "2.19.21"

\header {
  texidoc = "Tuplet numbers associated with kneed beams will
avoid accidentals."
}

\layout {
  indent = 0
  ragged-right = ##t
}

top = \change Staff = "1"
bottom = \change Staff = "2"

music = \relative {
  \override Beam.auto-knee-gap = 1
  \tuplet 5/4 4 {
    c16[ \top g'' e' \bottom fis,, \top c']
    e'16 \bottom g,, c, \top <eis' gis cis> \bottom g,
  }
  \tuplet 3/2 4 {
    \top c8 c'' ceses,,
    g'' g,, <aeses' ceses eses>
  }
}

\score {
  \new PianoStaff <<
    \new Staff = "1" {
      s1
    }
    \new Staff = "2" {
      \clef bass
      \music
    }
  >>
}
