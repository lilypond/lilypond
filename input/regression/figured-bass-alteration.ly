
\header {
  texidoc = "Bass figures can carry alterations."
}

\version "2.19.21"

\layout {
  ragged-right= ##t
}

\relative {
  \new Voice 
  <<
    { c''4 c c }
    \figures
    {
      <3- > <3! > <3+ >
    }
  >>
}
    
