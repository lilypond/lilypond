\version "2.19.21"

\header {
  texidoc = "Tuplet numbers are positioned correctly on kneed French-style beams."
}

\layout {
  indent = 0
  ragged-right = ##t
}

top = \change Staff = "1"
bottom = \change Staff = "2"

music =
\relative {
  \time 3/4
  \override Beam.auto-knee-gap = 1
  \override Stem.french-beaming = ##t
  \override TupletBracket.bracket-visibility = ##f
  \set subdivideBeams = ##t
  \set baseMoment = #(ly:make-moment 1 8)
  \tuplet 3/2 8 {
    g16 e''' c e g,,, c
    \bottom c,16. \top c''32 \bottom c,,16
    \top c''16. \bottom c,,32 c,16
  }
  \tuplet 5/4 8 {
    c'32 \top c''' \bottom c,,,, \top c''' \bottom c,,
    \top c'''32 \bottom c,,,, \top c''' \bottom c,, \top c'''
  }
}

\new PianoStaff <<
  \new Staff = "1" {
    \music
  }
  \new Staff = "2" {
    \clef bass
    s2.
  }
>>
