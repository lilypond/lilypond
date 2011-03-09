\version "2.13.53"

\header{
texidoc="

If the @code{Rest_engraver} is replaced by the @code{Completion_rest_engraver},
rests with a duration factor still keep their requested appearance.

"
}

#(set-paper-size "a6")

\layout { ragged-right= ##t }


\new Voice \with {
    \remove "Rest_engraver"
    \consists "Completion_rest_engraver"
} \relative c'{

  r\breve |
  r1*2 |
  r2*4 |
  r8*20
}
