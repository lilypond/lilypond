\version "2.19.21"

\header { texidoc = "Empty chords accept articulations, occupy no time,
                     and leave the current duration unchanged."
        }

\relative {
  r4 e'8( g <>) ^"sul D" \f \> \repeat unfold 8 { c-. } <>\sfz
  <>\downbow \repeat unfold 2 { c g } c1\> <>\enddecr
}
