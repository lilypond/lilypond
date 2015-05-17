\version "2.19.21"

\header {

  texidoc = "For other tunings, it is sufficient to set
    @code{stringTunings}. The number of staff lines is adjusted
    accordingly."

}


\new TabStaff {
  \set TabStaff.stringTunings = \stringTuning <gis'' dis'' ais' f'>
  \relative  { c''4 d e f }
}


