\version "2.4.0"
\header {
texidoc = "Also in the nested syntax here, grace notes appear rightly."
}

\score {  \context Voice \relative c'' {

    <<
     { \grace  g32 f4 }
    >>
    \grace c16 c2. \bar "|."
}
  \layout { raggedright = ##t }
	 
}



