
\version "2.16.0"
\header{
	texidoc="@cindex Beam Isknee

Beams can be placed across a @code{PianoStaff}.

"

}

\layout{
  ragged-right = ##t
}

\context PianoStaff <<
  \new Staff =  "one" \relative c'{
    s1
  }
  \new Staff =  "two" \relative c'{
    \clef bass
				% no knee
    \stemUp  c8[ \change Staff=one \stemDown g'16 f]
    s8
    s2
  }
>>



