\version "2.16.0"

\header{
texidoc="

If the @code{Note_heads_engraver} is replaced by the @code{Completion_heads_engraver}, notes that cross bar lines are split into tied notes.

"
}

\layout { ragged-right= ##t }


\new Voice \with {
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
} \relative c'{
  \time 2/4

  c2. c8 d4 e f g a b c2.. b8 a g16 f4 e d c8. c2 
}
