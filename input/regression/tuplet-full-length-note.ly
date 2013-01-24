\header {


  texidoc = "tuplet can be made to run to prefatory matter or
the next note, by setting @code{tupletFullLengthNote}."

  }

\version "2.17.11"

\new RhythmicStaff {
  \set tupletFullLength = ##t
  \time 4/4
  \tuplet 5/4 {
    c'4 c'1
  }
  \set tupletFullLengthNote = ##t
  \time 2/4
  \tuplet 3/2 {
    c4 c c 
  }
  \time 3/4
  c'4 c'4 c'4
}
