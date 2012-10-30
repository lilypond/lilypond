
\version "2.17.6"
\header{
  texidoc="
Arpeggios are supported, both cross-staff and broken single staff.
"
}

\layout { ragged-right= ##t }


\context PianoStaff << 
  \new Staff \relative c''{
    <fis,  d a>\arpeggio
    \arpeggioArrowUp
    <fis d a >\arpeggio	    
    %%\override PianoStaff.SpanArpeggio.connect = ##t
    \set PianoStaff.connectArpeggios = ##t
    <fis d a>\arpeggio
  }
  \new Staff\relative c{
    \clef bass
    <g b d>\arpeggio
    \arpeggioArrowDown
    <g b d>\arpeggio
    <g b d>\arpeggio
  }
>>
