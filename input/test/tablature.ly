\version "1.5.68"

%{

A sample tablature, with both normal staff and tab.

Tablature is done by overriding the note-head formatting function, and
putting it on a 6-line staff. A special engraver takes care of going
from string-number + pitch to number.

%}

partition = \notes {
    a,4-2 c'-5 a-4 e'-6
    e-3 c'-5 a-4 e'-6
  }

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
