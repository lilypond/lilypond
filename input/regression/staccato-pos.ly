
\version "2.3.4"

\header{

    texidoc=" The staccato dot (and all scripts with follow-into-staff
set) must not be on staff lines. The staccato dot is close to the
notehead. If the head is in a space, then the dot is in the space next
to it.  "

}

\score { 
    \context Voice \relative c' {
	e'4-. f-. g-. d-. c-. b-.
	\stemDown
	e,-. d-. c-. b-. a-. g-.    
    }
    \paper {
	raggedright = ##t
    }  
}

