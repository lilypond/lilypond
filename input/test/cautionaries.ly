\version "1.7.18"
\header{
	texinfo="@cindex Cautionary Accidentals
Lilypond can display cautionary accidentals in different ways.
" }
\score { \notes \context Staff \transpose c c'' {
  \key d \major
%  \property Staff.autoReminders = #'cautionary
  \property Staff.Accidental \override #'font-relative-size = #0
  <<dis c>>1 cis?2 d?
  \property Staff.Accidental \override #'cautionary-style = #'smaller
  <<dis c>>1 cis?2 d?
  \property Staff.Accidental \override #'cautionary-style = #'parentheses
  <<dis c>>1 cis?2 d?

}
}

%% new-chords-done %%
