\version "2.1.22"
\header {
texidoc = "Also in the nested syntax here, grace notes appear rightly."
}

\score { \notes \context Voice \relative c'' {

    <<
     { \grace  g32 f4 }
    >>
    \grace c16 c2. \bar "|."
}
  \paper { raggedright = ##t }
	 
}



