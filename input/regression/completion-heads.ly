\version "1.9.8"

\header{
texidoc="

If the Note_heads_engraver is replaced by the
@code{Completion_heads_engraver}, notes that cross bar lines are split
into tied notes.

"
}

\paper { raggedright= ##t }



\score{
  \notes\relative c'{
  \time 2/4

  c2. c8 d4 e f g a b c2.. b8 a g16 f4 e d c8. c2 
  }
  \paper{
    \translator{
      \ThreadContext
      \remove "Note_heads_engraver"
      \consists "Completion_heads_engraver"
    }
  }
}
