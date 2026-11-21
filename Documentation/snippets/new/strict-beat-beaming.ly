\version "2.27.6"

\header {
  categories = "Rhythms"

  texidoc = "
By default, fractional beams are printed to avoid having beams cross beat
boundaries.  This policy may be relaxed to reduce the number of fractional
beams, or tightened to show sub-beat rhythmic importance too.
"

  doctitle = "Strict beat beaming"
}

exampleRhythm = {
  \time 2/4
  8.->[ 16 16-> 16 8]
  \time 6/8
  8.->[ 16 16 16] 4.->
}

<<
  \new RhythmicStaff \with { instrumentName = "#f" } {
    \set strictBeatBeaming = ##f
    \exampleRhythm
  }
  \new RhythmicStaff \with { instrumentName = "default" } {
    \exampleRhythm
  }
  \new RhythmicStaff \with { instrumentName = "#t" } {
    \set strictBeatBeaming = ##t
    \exampleRhythm
  }
>>
