
\version "2.1.7"
\header{
	texidoc="@cindex Beam Isknee
LilyPond can beam across a Piano Staff.
" }
\score{
	\context PianoStaff <<
	\context Staff=one \notes\relative c'{
		s1
	}
	\context Staff=two \notes\relative c'{
		\clef bass
% no knee
		\stemUp  c8[ \change Staff=one \stemDown g'16 f]
		s8
		s2
	}
	>>
	\paper{
 		raggedright = ##t
	}
}



