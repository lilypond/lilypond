\version "1.6.2"

%{

A sample tablature, with both normal staff and tab.

Tablature is done by overriding the note-head formatting function, and
putting it on a 6-line staff. A special engraver takes care of going
from string-number + pitch to number.

%}

partition = \notes {
    \key e \major
    e8\5 fis\5 gis\5 a\5 b\5 cis'\5 dis'\5 e'\5
    e8\4 fis\4 gis\4 a\4 b\4 cis'\4 dis'\4 e'\4
    e8 fis gis a b cis' dis' e'
    \property Score.minimumFret = #5
    e8 fis gis a b cis' dis' e'
}

\score {
  \context StaffGroup <
    \context Staff <
	\clef "G_8"
	\partition
    >
    \context TabStaff <
	\partition
    >
  >
}
