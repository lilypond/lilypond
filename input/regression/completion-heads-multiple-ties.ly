\version "2.16.0"

\header{
texidoc="
The @code{Completion_heads_engraver} correctly handles notes that need to be split into more than 2 parts.
"
}

\layout { ragged-right= ##t }


\new Voice \with {
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
} \relative c'{
  \time 2/4
  c4.. c4. c4. c2 c1
}
