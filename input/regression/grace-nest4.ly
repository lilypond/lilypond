\version "2.11.51"
\header {
  texidoc = "Also in the nested syntax here, grace notes appear rightly."
}

\layout { ragged-right = ##t }

\context Voice \relative c'' {

  <<
    { \grace  g32 f4 }
  >>
  \grace c16 c2. \bar "|."
}





