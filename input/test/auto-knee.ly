
\score {
	  \context Staff \notes\relative c'{ 
%	  	\property Voice.autoKneeGap = #13
	  	[c8 e'] [c' c,,]
	}
	\paper{
		\translator{
			\StaffContext
			autoKneeGap = #13
		}
	}
}
