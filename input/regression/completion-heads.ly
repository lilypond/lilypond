\version "1.5.68"

\header{
texidoc="

If the Note_heads_engraver is replaced by the Completion_heads_engraver, 
notes that cross bar lines are split into tied notes.
"
}

\score{
  \notes\relative c'{
  \time 2/4

  c2. c8 d4 e f g a b c8 c2 b4 a g16 f4 e d c8. c2 
  }
  \paper{
    \translator{
      \ThreadContext
      \remove "Note_heads_engraver"
      \consists "Completion_heads_engraver"
    }
  }
}