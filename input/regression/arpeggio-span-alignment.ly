\version "2.27.2"

\header {
  texidoc = "When using span arpeggios with different side position support
(e.g., due to accidentals or head placement) these individual arpeggios
are horizontally aligned to avoid collisions with preceding grobs."
}

\new PianoStaff <<
  \set PianoStaff.connectArpeggios = ##t
  \new Staff \relative c'' {
    <geses aeses beses ceses deses eses feses geses>4\arpeggio r r2 |
    r2 <geses aeses beses ceses deses eses feses geses>2\arpeggio |
    \break
    <geses aeses beses ceses deses eses feses geses>2\arpeggio
  }

  \new Staff \relative c' {
    c4\arpeggio r4 r2 |
    \repeat unfold 8 c16 c2\arpeggio |
    \break
    c2\arpeggio
  }
>>

\new PianoStaff <<
  \set PianoStaff.connectChordBrackets = ##t
  \new Staff \relative c'' {
    r2 <geses aeses beses ceses deses eses feses geses>2\nonArpeggiato
  }

  \new Staff \relative c' {
    \repeat unfold 8 c16
    c2\nonArpeggiato |
  }
>>



\new PianoStaff <<
  \set PianoStaff.connectChordSlurs = ##t
  \new Staff \relative c'' {
    r2 <geses aeses beses ceses deses eses feses geses>2\chordSlur
  }

  \new Staff \relative c' {
    \repeat unfold 8 c16
    c2\chordSlur |
  }
>>
