#(ly:set-option 'old-relative)
\version "1.9.0"
\header {
  texidoc = "Grace note spacing. Should be tuned? "
}
	
\score {
 \notes \context Voice \relative c'' { \grace {  c16-[ d] } c4 }
  \paper { raggedright = ##t}

}

