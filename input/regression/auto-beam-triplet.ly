
\header
{
  texidoc = "Automatic beaming is also done on tuplets."
}

\version "2.6.0"

\relative c''{
  c8 c c c
  \times 4/6 { c c c c c c}
}
\layout { raggedright= ##t }
