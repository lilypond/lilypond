\header { texidoc = "A temporary ossia in an instrumental part for
    just a few bars on a separate staff. Here is an example using
    proper short staffs. A simpler solution is to instantiate a full
    staff, and let @code{RemoveEmptyStaffContext} take out the unused parts.

   Both solutions are demonstrated here.
" }

\version "2.1.26"

\score {
    \notes\relative c''
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
    

    \paper {
	raggedright= ##t
	\translator {\RemoveEmptyStaffContext}
	\translator {
	    \ScoreContext
	    \remove System_start_delimiter_engraver
	    }
    }
}

