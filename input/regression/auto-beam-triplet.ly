
\header
{
  texidoc = "Automatic beaming is also done on tuplets."
}

\version "2.17.11"
\layout { ragged-right= ##t }

\relative c''{
  c8 c c c
  \tuplet 6/4 { c c c c c c}
}
