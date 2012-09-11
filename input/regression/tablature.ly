\version "2.16.0"

\header{ texidoc = "@cindex Tabulature
A sample tablature, with both normal staff and tab.

Tablature is done by overriding the note-head formatting function, and
putting it on a 6-line staff. A special engraver takes care of going
from string-number + pitch to number.

String numbers can be entered as note articulations (inside a chord) and
chord articulations (outside a chord)
"
       }

partition =  {
  \key e \major
  <e\5 dis'\4>
  <e dis'>
  <e dis'\4>
  <e dis'>\5\4
  <e dis'\4>\5
}


\context StaffGroup <<
  \context Staff <<
    \clef "G_8"
    \partition
  >>
  \context TabStaff <<
    \partition
  >>
>>


