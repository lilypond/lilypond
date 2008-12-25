\version "2.12.0"

\header {
  texidoc = "Voices that go back and forth between staves do not confuse the spacing engine."
}

\paper { ragged-right = ##t }

\new PianoStaff <<
        \new Staff = "rh" \relative c'' {
                \time 6/8
                bes16 c d
                \change Staff = lh
                \stemUp bes a g
                \change Staff = rh
                \stemDown bes c d
                \change Staff = lh
                \clef bass
                \stemUp bes, a g

        }
        \new Staff = "lh" \relative c' {
                s2.
        }
>>