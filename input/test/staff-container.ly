\version "1.7.18"

\header {

    texidoc = "

Container By splitting the grouping (Axis_group_engraver) and creation
functionality into separate contexts, you can override interesting
things.

Notation like this is used in modern scores. Note that Lilypond is not
especially prepared for it: the clefs and time-signatures don't do
what you would expect.

    "

      }



%% 
%% s4 would create a staff.
%%
quarterSkip = #(make-nonevent-skip (ly:make-duration 2 0))

\score  {
 \notes \relative c'' <
 	\context StaffContainer = SA {

	    %% need < >, otherwise we descend to the voice inside SA  
	    < \context Staff = SA { c4 c4 } >
	    \quarterSkip
	    
	    < \context Staff = SB { b4 b4 } > 
	}
 	\context StaffContainer =SB {
	    \quarterSkip
	    < \context Staff { e d f } >
	    \quarterSkip
	}
 >

\paper {
	\translator {
		\ScoreContext
		\accepts StaffContainer
		\denies Staff
	}
	\translator {
		\type Engraver_group_engraver
		\consists Clef_engraver
		\consists Time_signature_engraver
		\consistsend "Axis_group_engraver"
		\accepts "Staff"
		
		\name StaffContainer
	}
	\translator {
		\StaffContext
		\remove Axis_group_engraver
		\remove Clef_engraver
		\remove Time_signature_engraver
	}
	raggedright=##t
}
}


