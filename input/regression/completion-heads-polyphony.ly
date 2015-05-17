\version "2.19.21"
\header {

    texidoc = "Completion heads are broken across bar lines. This was
intended as a debugging tool, but it can be used to ease music entry.
Completion heads are not fooled by polyphony with a different rhythm.
"

}

\layout {
  \context{
    \Voice
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
  }
}

\context Staff \relative << 
  { c''2. c bes2 } \\
  { c2. a8 g4 f4. g4 f  }
>>
