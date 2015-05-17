
\version "2.19.21"

\header{
  texidoc="
Beams should look the same.
"
}
\layout { ragged-right= ##t }


\context Voice \relative {
  d''8[ d d]  d[ g d]
  c c
}
