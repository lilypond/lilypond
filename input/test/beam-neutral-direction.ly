#(ly:set-option 'old-relative)
\version "1.9.1"
\header{
	texidoc="@cindex Beam Neutral Direction
When a beam falls in the middle of the staff, LilyPond normally
prints the beam pointing down.  However, this behaviour can be
altered if desired.
" }

fragment = \notes {
   b''8[ b]
  \property Voice.Beam \set #'neutral-direction = #-1
   b[ b]
  \property Voice.Beam \set #'neutral-direction = #1
   b[ b]
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper {raggedright = ##t }  
}

