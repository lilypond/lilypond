
\version "2.6.0"

\header{
  texidoc="
Beams should look the same.
"
}
\layout { raggedright= ##t }


\context Voice \relative c {
  d''8[ d d]  d[ g d]
  c c
}
