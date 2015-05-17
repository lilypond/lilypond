\version "2.19.21"
\header {
  texidoc =
  "Open and black note heads are not merged by default."
  
}

\layout { ragged-right= ##t }


\context Staff  \relative <<
  {
    c''2 c8 c4.
  }\\
  {
    c8 c4. c2
  }
>>
