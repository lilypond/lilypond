
\version "2.3.16"
\header{
       texidoc = "
The line-spanners connects to the Y position of the note  on the next line.
When put across line breaks, only the part before the line break is
printed. 
"

}

\score{
        \context PianoStaff <<
	    \set PianoStaff.followVoice = ##t	    
                \context Staff=one \relative c''{
		    a1 \break
		    \change Staff=two
		    a,
		}
        \context Staff=two { \clef bass \skip 1*2 }
    >>
    \paper{
	raggedright = ##t
    }
}
