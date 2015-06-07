\version "2.19.21"

\header {
  texidoc = "Tuplet numbers are placed next to kneed beams when
@code{Beam.positions} is overridden."
}

\layout {
  indent = 0
  ragged-right = ##t
}

top = \change Staff = "1"
bottom = \change Staff = "2"

music = \relative {
  \override Beam.auto-knee-gap = 1
  \tuplet 3/2 4 {
    c8 \top e'' \bottom g,,
    \once \override Beam.positions = #'(4.5 . 4.5)
    c,8 \top e'' \bottom g,,
    \once \override Beam.positions = #'(-7.0 . -7.0)
    \top e''8 \bottom c,, c,
    \once \override Beam.positions = #'(-4.5 . -4.5)
    \top e'''8 \bottom c,, c,
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
