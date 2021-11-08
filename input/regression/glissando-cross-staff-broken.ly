\version "2.23.5"

\header {
  texidoc = "Cross-staff glissandi have acceptable slopes when
they cross line breaks."
}

\paper {
  ragged-right = ##t
}

\new PianoStaff <<
  \new Staff = upper { s1*3 }
  \new Staff = lower {
    \clef bass
    \override Glissando.breakable = ##t
    \override Glissando.after-line-breaking = ##t
    d1\glissando
    \break
    \change Staff = upper
    f'''1\glissando
    \break
    \change Staff = lower
    d1
  }
>>
