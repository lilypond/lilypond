\version "2.3.8"
\header{
	texidoc="@cindex Cautionary Accidentals
Cautinary accidentals are displayed in slurs by default. They can be 
shown also with accidentals of smaller size.
" }
\score {  \context Staff \transpose c c'' {
  \key d \major
%  \set Staff.autoReminders = #'cautionary
  \override Staff.Accidental  #'font-size = #0
  <dis c>1 cis?2 d?
  \override Staff.Accidental  #'cautionary-style = #'smaller
  <dis c>1 cis?2 d?
  \override Staff.Accidental  #'cautionary-style = #'parentheses
  <dis c>1 cis?2 d?

}
\paper{raggedright = ##t}
}


