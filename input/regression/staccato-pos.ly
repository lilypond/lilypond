
\version "1.9.8"

\header{
texidoc="
The staccato dot (and all scripts with follow-into-staff set), must
not be on staff lines.
"
}
\score { 
  \context Voice \notes\relative c' {
	e'4-. f-. d-. c-. b-. 
  }
  \paper {
    raggedright = ##t
  }  
}

