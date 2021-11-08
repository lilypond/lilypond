\version "2.23.5"

\header {
  texidoc = "Broken cross-staff glissandi have acceptable slopes when
one staff is removed."
}

\paper {
  ragged-right = ##t
}

\layout {
  \context {
    \Staff
    \RemoveAllEmptyStaves
  }
}

\markup \vspace #2

<<
  \new Staff = upper { s1*3 }
  \new Staff = lower {
    \clef bass
    \override Glissando.breakable = ##t
    \override Glissando.after-line-breaking = ##t
    d1\glissando
    \break
    \change Staff = upper
    f'''1\glissando
    \change Staff = lower
    c1
  }
>>
