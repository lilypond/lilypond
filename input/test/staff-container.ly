\version "2.1.7"

\header {

    texidoc = "

Container By splitting the grouping (Axis_group_engraver) and creation
functionality into separate contexts, you can override interesting
things.

Notation like this is used in modern scores. Note that LilyPond is not
especially prepared for it: the clefs and time-signatures don't do
what you would expect.

    "

      }



%% 
%% s4 would create a staff.
%%
quarterSkip = #(make-nonevent-skip (ly:make-duration 2 0))

\score  {
 \notes \relative c'' <<
 	\new StaffContainer {

	    %% need << >>, otherwise we descend to the voice inside SA  
	    << \new Staff { c4 c4 } >>
	    \quarterSkip
	    
	    << \new Staff { b4 b4 } >> 
	}
 	\new StaffContainer {
	    \quarterSkip
	    << \context Staff { e d f } >>
	    \quarterSkip
	}
 >>

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


