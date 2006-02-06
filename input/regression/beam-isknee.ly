
\version "2.7.32"
\header{
	texidoc="@cindex Beam Isknee

Beams can be placed across a @code{PianoStaff}.

"

}

\layout{
  ragged-right = ##t
}

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



