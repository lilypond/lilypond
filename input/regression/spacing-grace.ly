
\version "2.3.16"
\header {
  texidoc = "Grace note spacing. Should it be tuned? "
}
	
\score {
  \context Voice \relative c'' { \grace {  c16[ d] } c4 }
  \paper { raggedright = ##t}

}

