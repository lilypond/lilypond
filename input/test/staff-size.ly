\version "1.5.68"
\score {
  \notes \relative c' < \context StaffContainer = SA{
     \property StaffContainer.StaffSymbol \set #'staff-space = #(/ 16 20)

	\property Staff.fontSize = #-1
	\property Voice.fontSize = #-1
	
	\dynamicUp\stemDown

	%\key gis \major
	c8 d [e f g a] b c \ff
  }

\context StaffContainer = SB {  \dynamicDown c,,4 \ff c c c  }

>
\paper { linewidth = -1. }
}

