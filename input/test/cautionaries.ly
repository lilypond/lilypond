\version "1.5.68"

\score { \notes \context Staff \transpose c''' {
  \key d \major
  \property Staff.autoReminders = #'cautionary
  \property Staff.Accidentals \override #'font-relative-size = #0
  <dis1 c> cis2 d
  \property Staff.Accidentals \override #'cautionary-size = #-1
  <dis1 c> cis2 d
  \property Staff.Accidentals \override #'paren-cautionaries = ##f
  <dis1 c> cis2 d

}
}

