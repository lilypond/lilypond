\version "2.19.21"
\header {
    texidoc = "Cross staff  (kneed) beams do not cause extreme slopes."
}

\layout { ragged-right = ##t}

\context PianoStaff <<
  \new Staff = "up"
  \relative <<
    {
      \stemDown
      f''16( d b \change Staff = down \stemUp
      \clef treble g ~ < g e>8)
      
      e[ \change Staff = up
	 \stemDown
	 e e]
      \change Staff = down
      \stemUp
      e8.[
	\change Staff = up
	\stemDown
	e8.]
      
    } \\
  >>
  \new Staff = "down" {
    \time 3/8 \clef bass s4.*3 }
>>

