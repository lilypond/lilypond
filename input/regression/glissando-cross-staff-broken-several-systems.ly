\version "2.23.5"

\header {
  texidoc = "Broken cross-staff glissandi can span more than
two systems."
}

\paper {
  ragged-right = ##t
}

\new PianoStaff <<
  \new Staff = upper { s1*5 }
  \new Staff = lower {
    \clef bass
    \override Glissando.breakable = ##t
    \override Glissando.after-line-breaking = ##t
    d1\glissando
    \break
    s1
    \break
    \change Staff = upper
    f'''1\glissando
    \break
    s1
    \break
    \change Staff = lower
    d1
  }
>>
