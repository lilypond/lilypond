\score {
  \notes \relative c' < \context Voice {
	\property Staff.staffLineLeading = 4
	\property Staff.fontSize = #-1
	\property Voice.fontSize = #-1
	
	\property Voice . dynamicDirectionection = \up \stemdown
%\key gis;
	c8 d [e f g a] b c \ff
  }

\context Staff = VB {  \property Voice . dynamicDirectionection = \down c,,4 \ff c c c  }

>
\paper { linewidth = -1.; }
}
\version "1.3.4"; 
