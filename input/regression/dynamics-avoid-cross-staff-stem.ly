\version "2.19.21"

\header {
  texidoc = "Cross-staff @code{Dynamic} does not trigger a cyclic
dependency for direction look-up.
"
}

<<
  \new Staff = "up"
    \relative {
      f'8
      \change Staff = "down"
      c e\f %should not trigger cyclic dependency
      \change Staff = "up"
      f
    }
  \new Staff = "down" { \clef bass s2 }
>>
