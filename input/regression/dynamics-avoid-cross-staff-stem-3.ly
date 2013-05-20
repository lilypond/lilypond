\version "2.17.17"

\header {
  texidoc = "Cross-staff @code{Dynamic} does not trigger a cyclic
dependency for direction look-up.
"
}

<<
  \new Staff = "up"
    \relative c' {
      f8
      \change Staff = "down"
      c e\f %should not trigger cyclic dependency
      \change Staff = "up"
      f
    }
  \new Staff = "down" { \clef bass s2 }
>>
