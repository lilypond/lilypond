\version "2.1.7"
\header {
texidoc = "Another combination of grace note nesting."
}

\score { \notes \context Voice \relative c'' {

    <<
     { \grace  g32 f4 }
    >>
    \grace c16 c2. \bar "|."
}
  \paper { raggedright = ##t }
	 
}



