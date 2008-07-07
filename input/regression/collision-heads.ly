\version "2.11.51"
\header {
  texidoc =
  "Open and black note heads are not merged by default."
  
}

\layout { ragged-right= ##t }


\context Staff  \relative c'' <<
  {
    c2 c8 c4.
  }\\
  {
    c8 c4. c2
  }
>>
