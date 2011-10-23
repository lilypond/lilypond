\version "2.15.16"

\header {

  texidoc = "For other tunings, it is sufficient to set
    @code{stringTunings}. The number of staff lines is adjusted
    accordingly."

}


\new TabStaff {
  \set TabStaff.stringTunings = \stringTuning <gis'' dis'' ais' f'>
  \relative c''  { c4 d e f }
}


