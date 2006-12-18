\header {

  texidoc = "In 'modern accidental style, the last note should
have an accidental sign. "
  
}
\version "2.10.3"

\score {
  \new Staff {
    \relative c' {
      #(set-accidental-style 'modern)
      d4 dis'4 d,2^"this should have accidental"
    }
  }
}
