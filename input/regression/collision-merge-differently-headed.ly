\version "2.12.0"
\header {
  texidoc =

  "If @code{merge-differently-headed} is enabled, then
open note heads may be merged with black noteheads, but only
if the black note heads are from 8th or shorter notes.
"
  
}

\layout { ragged-right= ##t }


\context Staff  \relative c'' <<
  {
    \override Staff.NoteCollision  #'merge-differently-headed = ##t
    c2 c8 c4.
    c2
  }\\
  {
    c8 c4. c2
    c4
  }
>>
