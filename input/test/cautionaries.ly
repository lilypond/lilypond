\version "2.1.22"
\header{
	texidoc="@cindex Cautionary Accidentals
LilyPond can display cautionary accidentals in different ways.
" }
\score { \notes \context Staff \transpose c c'' {
  \key d \major
%  \set Staff.autoReminders =  #'cautionary
  \override Staff.Accidental  #'font-size = #0
  <dis c>1 cis?2 d?
  \override Staff.Accidental  #'cautionary-style = #'smaller
  <dis c>1 cis?2 d?
  \override Staff.Accidental  #'cautionary-style = #'parentheses
  <dis c>1 cis?2 d?

}
\paper{raggedright = ##t}
}


