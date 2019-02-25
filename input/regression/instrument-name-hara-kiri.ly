\version "2.21.0"

\header {
  texidoc = "Instrument names are removed when the staves are
killed off.

In this example, the second staff (marked by the bar number 2)
disappears, as does the instrument name."
}
        
\new PianoStaff <<
  \new Staff {
    \override Staff.VerticalAxisGroup.remove-empty = ##t  
    \set PianoStaff.instrumentName = "up"
    \set PianoStaff.shortInstrumentName = "u"
    c'1\break R
  }
>>
