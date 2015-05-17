\header {
  texidoc = "Span arpeggios within one staff also work"
  }

\version "2.19.21"
\layout { ragged-right = ##t }

\new PianoStaff <<
 \set PianoStaff.connectArpeggios = ##t
 \new Staff \relative {
   <<
     {
       c''2\arpeggio
     }
     \\
     {
       g2\arpeggio a
     }
   >>
 }
>>
