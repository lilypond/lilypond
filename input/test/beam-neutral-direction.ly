
\version "2.2.0"
\header{
	texidoc="@cindex Beam Neutral Direction
When a beam falls in the middle of the staff, the beams point normally 
down.  However, this behaviour can be altered, if desired.
" }

fragment = \notes {
   b''8[ b]
  \override Beam  #'neutral-direction = #-1
   b[ b]
  \override Beam  #'neutral-direction = #1
   b[ b]
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper {raggedright = ##t }  
}

