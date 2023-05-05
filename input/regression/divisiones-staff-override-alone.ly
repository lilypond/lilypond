\version "2.25.5"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This test exercises ancient divisions (divisiones) with
settings that are overridden in various built-in @code{Staff}
contexts.  Each @code{Staff} is in a separate @code{\\score}."
}

\include "divisiones-staff-override-music.ily"
\include "divisions-staff-override-staves.ily"

\score { \staffA }
\score { \staffB }
\score { \staffC }
\score { \staffD }
\score { \staffE }
\score { \staffF }
