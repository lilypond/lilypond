\version "2.19.21"

\header {
  texidoc = "Voices that go back and forth between staves do not confuse the spacing engine."
}

\paper { ragged-right = ##t }

\new PianoStaff <<
        \new Staff = "rh" \relative {
                \time 6/8
                bes'16 c d
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

\new PianoStaff <<
        \new Staff = "rh" \relative {
                \time 6/8
                bes'16 c d
                \change Staff = lh
                \stemUp bes a g
                \change Staff = rh
                \stemDown bes c d
                \change Staff = lh
                \clef bass
                \stemUp ces, a g

        }
        \new Staff = "lh" \relative c' {
                s2.
        }
>>
