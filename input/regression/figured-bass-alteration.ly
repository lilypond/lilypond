
\header {
  texidoc = "Bass figures can carry alterations."
}

\version "2.12.0"

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
    
