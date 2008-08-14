
\header
{
  texidoc = "Automatic beaming is also done on tuplets."
}

\version "2.11.51"
\layout { ragged-right= ##t }

\relative c''{
  c8 c c c
  \times 4/6 { c c c c c c}
}
