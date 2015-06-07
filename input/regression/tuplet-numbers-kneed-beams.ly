\version "2.19.21"

\header {
  texidoc = "Tuplet numbers are positioned next to kneed beams."
}

\layout {
  indent = 0
  ragged-right = ##t
}

top = \change Staff = "1"
bottom = \change Staff = "2"

music = \relative {
  \time 3/4
  \override Beam.auto-knee-gap = 1
  \tuplet 3/2 4 {
    c8 g' \top e'
    c'8 e, \bottom g,
    \top e''8 \bottom c,, \top g''
    g,8 e''' c,,
    c''8 g,, e'
    g,8 c''' e,,
  }
}

\score {
  \new PianoStaff <<
    \new Staff = "1" {
      s2.
      s2.
    }
    \new Staff = "2" {
      \clef bass
      \music
    }
  >>
}
