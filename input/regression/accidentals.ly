\version "2.2.0"

\header{
texidoc="
This shows how accidentals are handled.
"
}
mel = \notes { \key d \major \time 4/4
 d4  dis dis8 dis, d4 | d dis disis8 d, dis4 | d des disis8 dis, d4 | dis deses d dis ~ | dis dis ~ dis8 d, dis4 ~ | \break
 dis dis cis c | c cis cisis cis | c ces cisis c | cis ceses c cis ~ | cis cis ~ cis cis \bar "|."  | \break
}

\score { \notes
 <<
  \context Staff \transpose c c'' \mel
  \context NoteNames{
      \set printOctaveNames = ##f
      \mel
      }
 >>
}

