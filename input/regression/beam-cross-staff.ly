\header{
texidoc="
Beams can be typeset over fixed distance aligned staffs, beam
beautification doesn't really work, but knees do. Beams should be
behave well, wherever the switching point is.
";
}
\score{
	\context PianoStaff <
	\context Staff=one \notes\relative c'{
		\stemUp [c8 c \translator Staff=two \stemUp c c]
		[c c c c]
		\translator Staff=one
		\stemDown [c8 c \translator Staff=two \stemUp c c]
		r2
		\stemDown [c8 c \translator Staff=one \stemDown c c]
		r2
		\translator Staff=two
		\stemUp [c8 c \translator Staff=one \stemDown c c]
		r2
	}
	\context Staff=two \notes\relative c'{
		\clef bass;
		s1
		s1
		s1
		s1
	}
	>
}


