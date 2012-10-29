\version "2.17.6"

\header {
  texidoc = "Spacing corrections for kneed beams still work when
compression is involved."
}

\paper { line-width = 14.4\cm }

\layout {
  \context {
    \Voice
    \override Beam.auto-knee-gap = #1
  }
}

rh = \change Staff = "rh"
lh = \change Staff = "lh"
\new PianoStaff <<
  \new Staff ="rh" {
    s1*3
  }
  \new Staff ="lh" \new Voice {
    \clef bass
    \repeat unfold 12 { \rh a'16 \lh d \rh a' \lh d \noBreak }
  }
>>
