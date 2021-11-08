\version "2.23.5"

\header {
  texidoc = "Broken cross-staff glissandi have acceptable slopes when
one staff is removed."
}

\paper {
  ragged-right = ##t
}

\markup \vspace #2

<<
  \new Staff = lower {
    \clef bass
    \override Glissando.breakable = ##t
    \override Glissando.after-line-breaking = ##t
    d1\glissando
    \break
    <<
      \new Staff = upper \with {
        alignAboveContext = lower
      }
      { s1 }
      {
        \change Staff = upper
        f'''1
      }
    >>
  }
>>
