\version "1.5.49"
\header{
texidoc="
The same goes for slurs. They behave decently when broken across
linebreak.
"
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
		\clef bass
		s1 s1 %s1 s1 s1 s1 s1 s1 s1 s1
	}
	>
	\paper { indent = 0. linewidth = 40.*\staffspace }
}


