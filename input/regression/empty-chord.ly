\version "2.25.35"

\header { texidoc = "Empty chords accept articulations, occupy no time,
                     and leave the current duration unchanged."
        }

\relative {
  r4 e'8( g <>) ^"sul D" \f \> \*8 c-. <>\sfz
  <>\downbow \*2 { c g } c1\> <>\enddecr
}
