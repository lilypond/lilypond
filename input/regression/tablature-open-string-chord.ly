\version "2.16.0"

\header {

  texidoc = "
Open strings can always be part of a chord in tablature, even when frets
above 4 have been used in the chord.  In this case, both chords should show
an open fourth string."

}  

\score {
  \new TabStaff { 
    \set TabStaff.stringTunings = \stringTuning <c g d' a'>
    <c g'> 1 <c\4 g'> 1
  }
}
