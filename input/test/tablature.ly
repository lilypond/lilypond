\version "1.3.146"

%{

A sample tablature, with both normal staff and tab.

Tablature is done by overriding the note-head formatting function, and
putting it on a 6-line staff. A special engraver takes care of going
from string-number + pitch to number.

%}

partition = \notes { ces'16^2 d'^2 e'8^2 g'2.^3  \times 2/3 { c'8^2 e'8^3 d'8^3 } e'4^2 }

\score {
  \context StaffGroup <
    \context Staff <
      % Hide fingering number (used for string number) for the "normal" staff
      \property Staff.Fingering \override #'transparent = ##t
      
      \partition
    >
    \context TabStaff <
      \partition
    >
  >
}
