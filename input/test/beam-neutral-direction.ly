
\version "2.1.26"
\header{
	texidoc="@cindex Beam Neutral Direction
When a beam falls in the middle of the staff, LilyPond normally
prints the beam pointing down.  However, this behaviour can be
altered if desired.
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

