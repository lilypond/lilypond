
\version "1.9.2"
\header {
  texidoc = "Grace note spacing. Should be tuned? "
}
	
\score {
 \notes \context Voice \relative c'' { \grace {  c16[ d] } c4 }
  \paper { raggedright = ##t}

}

