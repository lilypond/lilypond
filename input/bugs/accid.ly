\header {
texidoc="The sharp is printed too far to the left."
}

\version "1.4.0"
\score {
\notes \context Staff <
  \context Voice = up {\stemUp gis''4}
  \context Voice = dwn {\stemDown <ces' d'>}
>
}
