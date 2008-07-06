
\header {

  texidoc = "@code{PianoStaff.instrument} and
@code{PianoStaff.instr} are removed when the staves are killed off.

In this example, the 2nd staff (marked by the barnumber 2) disappears
as does the instrument name.  "

}

\version "2.11.51"
        
  \new PianoStaff <<
    \new Staff {
      \override Staff.VerticalAxisGroup #'remove-empty = ##t  
      \set PianoStaff.instrumentName ="up" 
      \set PianoStaff.shortInstrumentName ="u"
      c'1\break R
    }
  >>
