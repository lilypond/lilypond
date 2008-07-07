
\header {
  texidoc = "Bass figures can carry alterations."
}

\version "2.11.51"

\layout {
  ragged-right= ##t
}

\relative c'' {
  \new Voice 
  <<
    { c4 c c }
    \figures
    {
      <3- > <3! > <3+ >
    }
  >>
}
    
