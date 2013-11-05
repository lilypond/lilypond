\version "2.19.0"

\header {
  texidoc = "Durations without pitches are placed into note events
without pitch information.  Those are directly useful in
@code{RhythmicStaff}."
}

\layout { ragged-right = ##t }

\new RhythmicStaff { 4 4. r | 4 \tuplet 3/2 { 2 4 } 4 }
