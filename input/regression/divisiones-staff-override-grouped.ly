\version "2.25.5"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This test exercises ancient divisions (divisiones) with
settings that are overridden in various built-in @code{Staff}
contexts.  All staves are in one @code{StaffGroup}."
}

\include "divisiones-staff-override-music.ily"
\include "divisions-staff-override-staves.ily"

\score { \staffGroup }
