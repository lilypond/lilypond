
\header {

 texidoc = "By splitting the grouping (Axis_group_engraver) and
creation functionality into separate contexts, you can override
interesting things. You can also drop the \consistsend feature.";

}


\score  {
 \notes <
 	\context StaffContainer = SA { \property StaffContainer.StaffSymbol \set
	  #'staff-space = #0.8
	  \context Staff { 	  c4 c4 } }
 	\context StaffContainer =SB { \context Staff { d f  } }
 >

\paper {
	\translator {
		\ScoreContext
		\accepts StaffContainer;
		\denies Staff;
	}
	\translator {
		\type Engraver_group_engraver;
		\consists "Axis_group_engraver";
		\accepts "Staff";
		\name StaffContainer;

	}
	\translator {
		\StaffContext
		\remove Axis_group_engraver;
	}
}
