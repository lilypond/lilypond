\version "2.25.34"

\header {
  categories = "Keyboards, Real music"

  texidoc = "
An non-arpeggiato bracket can indicate that notes on two different staves
are to be played with the same hand.  In order to do this, the
@code{PianoStaff} must be set to accept cross-staff brackets.

The following example typesets measure@tie{}65 of Debussy's prelude
@emph{Les collines d'Anacapri}.
"

  doctitle = "Indicating cross-staff chords with a bracket"
}


\new PianoStaff <<
  \once \set PianoStaff.connectChordBrackets = ##t

  \new Staff \relative c' {
    \key b \major
    \time 6/8
    b8-.(\nonArpeggiato fis'-.\> cis-.
      e-. gis-. b-.)\!\fermata^\laissezVibrer
    \section
  }

  \new Staff \relative c' {
    \clef bass
    \key b \major
    << { <a e cis>2.\nonArpeggiato } \\
       { <a, e a,>2. } >>
    \section
  }
>>
