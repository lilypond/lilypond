\version "2.14.0"

\header {

  texidoc = "For other tunings, it is sufficient to set
    @code{stringTunings}. The number of staff lines is adjusted
    accordingly."

}


\new TabStaff {
  \set TabStaff.stringTunings = #`(,(ly:make-pitch 0 3 0)
				   ,(ly:make-pitch 0 5 SHARP)
				   ,(ly:make-pitch 1 1 SHARP)
				   ,(ly:make-pitch 1 4 SHARP))
  \relative c''  { c4 d e f }
}


