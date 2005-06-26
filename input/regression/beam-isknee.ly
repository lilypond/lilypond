
\version "2.6.0"
\header{
	texidoc="@cindex Beam Isknee

Beams can be placed across a @code{PianoStaff}.

"

}
\score{
	\context PianoStaff <<
	\context Staff = "one" \relative c'{
		s1
	}
	\context Staff = "two" \relative c'{
		\clef bass
% no knee
		\stemUp  c8[ \change Staff=one \stemDown g'16 f]
		s8
		s2
	}
	>>
	\layout{
 		raggedright = ##t
	}
}



