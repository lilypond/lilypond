\header { texidoc = "A temporary ossia in an instrumental part may
    be printed using a separate, short staff. A simpler solution is 
    also given: instantiate a full staff, and let 
    @code{RemoveEmptyStaffContext} take out the unused parts.
" }

\version "2.3.22"

\score {
    \relative c''
    \new StaffGroup \with {
	\remove "System_start_delimiter_engraver"
	\override SpanBar #'glyph = #":"
    } <<

	%% solution 1
	{ c1 c1
	<<
	    { c1 c1 } 
	    \new Staff \with {
		\remove "Time_signature_engraver"
	    } {
		c,4^"ossia" es f fis g1
	    }
	>>
	  c1 \break c c }

	%% solution 2
	\new Staff { R1*2 c,4^"ossia" es f fis g1 R1 * 3 }
    >>
    

    \layout {
	raggedright= ##t
	\context {\RemoveEmptyStaffContext}
	\context {
	    \Score
	    \remove System_start_delimiter_engraver
	    }
    }
}

