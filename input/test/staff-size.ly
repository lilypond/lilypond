
\header { texidoc = "Setting staff sizes is a little clumsy.  There
are two options: using StaffContainer and override/revert, or
\outputproperty. Both methods are shown in this example."
}

\version "1.5.70"
\score {
  \notes \relative c' < \context StaffContainer = SA{
     \property StaffContainer.StaffSymbol \set #'staff-space = #(/ 16 20)

	\property Staff.fontSize = #-1
	\property Voice.fontSize = #-1
	
	\dynamicUp\stemDown

	%\key gis \major
	c8 d [e f g a] b c \ff
  }

\context Staff = SB { \dynamicDown c,,4 \ff c c c  }
\context Staff = SC {
  \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
    #'staff-space =  #0.8
  \property Staff.fontSize = #-1
  \clef bass
  c8 c c c  c c c c
}
>
\paper { linewidth = -1. }
}

