


\score {  \notes
	\relative c'' \context Staff {
		\property Staff.textEmptyDimension = "1"
		\repeat "semi" 2 { \time 4/4; c4^"cuivr\\'e"_\fermata }
		 \alternative {
		 	{ \property Voice.textStyle = "italic" d-4_"cantabile" }
		 	{  e }  } \grace { c16 }
			\property Voice.textStyle = "large"
			 f4-\ff^""^"Largo" \mark "B"; g 
	}
	\paper { linewidth = -1.0;
	\translator { \BarNumberingStaffContext}
	}
}
