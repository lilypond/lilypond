\version "1.3.146"

%{

A sample tablature.

Tablature is done by overriding the note-head formatting function, and
putting it on a 6-line staff. A special engraver takes care of going
from string-number + pitch to number.

%}

\score {
  \notes  \context TabStaff { ces'16-2 d'-2 e'8-2 g'2.-3  c'4-1 c''8-5 b'-5 }
  
 }
