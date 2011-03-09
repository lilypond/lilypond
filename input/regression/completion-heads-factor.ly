\version "2.12.0"

\header{
texidoc="

If the @code{Note_heads_engraver} is replaced by the @code{Completion_heads_engraver},
notes with a duration factor still keep their requested appearance.

"
}

#(set-paper-size "a6")

\layout { ragged-right= ##t }


\new Voice \with {
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
} \relative c'{

  c\breve |
  c1*2 |
  c2*4 |
  c8*20
}
