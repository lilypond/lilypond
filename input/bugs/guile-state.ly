\header{
texidoc="Real header: the slurBeginAttachment setting still lives in the
second example, breaking the cross staff broken slur.
";
}
	
\header{
texidoc="
Slurs can be forced to always attach to note heads.
";
}


\score{
	\notes \relative c''{
		\property Voice.VerticalDirection = #1
		\property Voice.slurBeginAttachment = #'head
		\property Voice.slurEndAttachment = #'head
		g16()g()g()g()d'()d()d()d
	}
	\paper{
		indent = 0.0;
		linewidth = 60.0\mm;
	}
}
	
\header{
texidoc="
The same goes for slurs. They behave decently when broken across
linebreak.
";
}

\score{
	\context PianoStaff <
	\context Staff=one \notes\relative c'{
%{
		\stemUp \slurUp
		 c4( c \translator Staff=two c )c |
		\translator Staff=one
		\stemUp \slurUp
		 c4( c \translator Staff=two c )c |
		\stemUp \slurUp
		 c4( c \translator Staff=one c )c |
		\translator Staff=two
		\stemUp \slurUp
		 c4( c \translator Staff=one c )c |
		\translator Staff=two
		\stemUp \slurUp
		 c4( \translator Staff=one c c )c |
%}
		r2
		\translator Staff=two
		\stemUp \slurUp
		 c4( \translator Staff=one c
		   \break
		c )c
		r2

%{
		\stemDown \slurDown
		 d4( \translator Staff=two c c \translator Staff=one )d
		\translator Staff=two
		\stemUp \slurUp
		 c4( \translator Staff=one c c \translator Staff=two )c
		r1
%}
	}
	\context Staff=two \notes\relative c'{
		\clef bass;
		s1 s1 %s1 s1 s1 s1 s1 s1 s1 s1
	}
	>
	\paper { indent = 0.; linewidth = 40.*\staffspace; }
}

\version "1.3.110"; 
