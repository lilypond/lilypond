
\version "2.3.8"
\header{
	texidoc="@cindex Beam Neutral Direction
When a beam falls in the middle of the staff, the beams point normally 
down.  However, this behaviour can be altered, if desired.
" }

fragment =  {
   b''8[ b]
  \override Beam  #'neutral-direction = #-1
   b[ b]
  \override Beam  #'neutral-direction = #1
   b[ b]
}

\paper { raggedright = ##t} 

\score {
  \relative c \fragment
  \paper {raggedright = ##t }  
}

