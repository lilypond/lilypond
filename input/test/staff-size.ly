\version "1.3.146"
\score {
  \notes \relative c' < \context Voice {
   \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
       #'staff-space = #(/ 16 20)

	\property Staff.fontSize = #-1
	\property Voice.fontSize = #-1
	
	\dynamicUp\stemDown

	%\key gis \major
	c8 d [e f g a] b c \ff
  }

\context Staff = VB {  \dynamicDown c,,4 \ff c c c  }

>
\paper { linewidth = -1. }
}

