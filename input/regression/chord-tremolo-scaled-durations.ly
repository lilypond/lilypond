\version "2.15.17"

\header {
  texidoc = "Don't allow scaled durations to confuse the tremolo beaming.
The tremolos should each have 3 beams."
}

{
  \time 3/4
  \repeat tremolo 12 {e'32 f'}
  \scaleDurations 3/4 {
    \repeat tremolo 12 {e'32 f'} r4
  }
}

