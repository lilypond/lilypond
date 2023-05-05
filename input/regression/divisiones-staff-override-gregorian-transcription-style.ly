\version "2.25.5"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="By default, @code{GregorianTranscriptionStaff} creates
@code{BarLine} grobs for @code{\\divisio}@dots{} commands, but
@code{\\EnableGregorianDivisiones} makes it create @code{Divisio}
grobs like the ancient-notation staves."
}

\include "divisiones-staff-override-music.ily"
\include "divisions-staff-override-gregorian-transcription-style-staves.ily"
