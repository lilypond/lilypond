
	% separate staff-size is clumsy with \override.
	
\score {
  \notes \relative c' < \context Voice {
	\property Staff.staffSpace = #10
	\property Staff.fontSize = #-1
	\property Voice.fontSize = #-1
	
	\property Voice . dynamicDirection = \up \stemDown
%\key gis \major;
	c8 d [e f g a] b c \ff
  }

\context Staff = VB {  \property Voice . dynamicDirection = \down c,,4 \ff c c c  }

>
\paper { linewidth = -1.; }
}

