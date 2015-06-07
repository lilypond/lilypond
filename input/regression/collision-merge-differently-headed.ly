\version "2.19.21"
\header {
  texidoc =

  "If @code{merge-differently-headed} is enabled, then
open note heads may be merged with black noteheads, but only
if the black note heads are from 8th or shorter notes.
"
  
}

\layout { ragged-right= ##t }


\context Staff  \relative <<
  {
    \override Staff.NoteCollision.merge-differently-headed = ##t
    c''2 c8 c4.
    c2
  }\\
  {
    c8 c4. c2
    c4
  }
>>
