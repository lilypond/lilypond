\version "2.24.1"

\header {
  texidoc = "A voice with @code{Ambitus_engraver} that starts
with a skip while another voice starts with a note does not cause
a programming error."
}

\new Staff <<
  \new Voice \with { \consists Ambitus_engraver } { \voiceOne R1 c'1 c''1 }
  \new Voice { \voiceTwo g4 }
>>
