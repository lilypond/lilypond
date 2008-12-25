\header {


  texidoc = "tuplet can be made to run to prefatory matter or
the next note, by setting @code{tupletFullLengthNote}."

  }

\version "2.12.0"

\new RhythmicStaff {
  \set tupletFullLength = ##t
  \time 4/4
  \times 4/5 {
    c'4 c'1
  }
  \set tupletFullLengthNote = ##t
  \time 2/4
  \times 2/3 {
    c4 c c 
  }
  \time 3/4
  c'4 c'4 c'4
}
