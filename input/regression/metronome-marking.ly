

\header {

  
  texidoc = "Here @code{\\tempo} directives are printed as metronome markings.



The marking is left aligned with the time signature, if there is one.
"
  
}

\layout {  raggedright = ##t }

\version "2.7.13"

\relative c'' {
  \tempo \breve = 100 c1 c1 \tempo 8.. = 50 c1
}




