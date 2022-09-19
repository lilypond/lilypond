\version "2.23.14"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This test exercises semantic divisions with settings that
are overridden in various built-in @code{Staff} contexts.  All staves
are in one @code{StaffGroup}."
}

\include "divisions-staff-override-music.ily"
\include "divisions-staff-override-staves.ily"

\score { \staffGroup }
