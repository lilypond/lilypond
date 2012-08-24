\version "2.16.0"

\header {
  texidoc = "Dot columns should not trigger vertical spacing before
line breaking.  If the regtest issues a programming_error saying that
vertical spacing has been called before line breaking, it has failed.
"
}

\context Staff <<
  \new Voice { \voiceOne f''8.[ e''16] }
  \new Voice { \voiceThree r8. a'16}
>>
