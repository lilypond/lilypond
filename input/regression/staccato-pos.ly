
\version "2.1.30"

\header{
texidoc="
The staccato dot (and all scripts with follow-into-staff set) must
not be on staff lines.
"
}
\score { 
  \context Voice \notes\relative c' {
	e'4-. f-. d-. c-. b-.
	\stemDown
	e,-. d-. c-. b-. a-. g-.    
  }
  \paper {
    raggedright = ##t
  }  
}

