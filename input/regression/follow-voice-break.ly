
\version "2.16.0"
\header{
  texidoc = "
The line-spanners connects to the Y position of the note  on the next line.
When put across line breaks, only the part before the line break is
printed. 
"

}

\layout{  ragged-right = ##t }



\context PianoStaff <<
  \set PianoStaff.followVoice = ##t	    
  \new Staff = "one" \relative c''{
    a1 \break
    \change Staff=two
    a,
  }
  \new Staff = "two" { \clef bass \skip 1*2 }
>>


