
\version "2.11.51"

\header{
  texidoc="
Beams should look the same.
"
}
\layout { ragged-right= ##t }


\context Voice \relative c {
  d''8[ d d]  d[ g d]
  c c
}
