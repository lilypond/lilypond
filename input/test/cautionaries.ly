\version "1.7.18"

%% FIXME

\score { \notes \context Staff \transpose c c'' {
  \key d \major
%  \property Staff.autoReminders = #'cautionary
  \property Staff.Accidental \override #'font-relative-size = #0
  <<dis c>>1 cis2 d
%  \property Staff.Accidental \override #'cautionary-size = #-1
  <<dis c>>1 cis2 d
%  \property Staff.Accidental \override #'paren-cautionaries = ##f
  <<dis c>>1 cis2 d

}
}

%% new-chords-done %%
