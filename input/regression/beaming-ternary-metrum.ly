
\version "2.19.21"

\header {

  texidoc = "Automatic beaming works also in ternary time sigs.
  As desired, the measure is split in half, with beats 1-3 and
  4-6 beamed together as a whole."

}

\layout { ragged-right = ##t}

\relative {
  \time 6/8
  c''8. c16 c16 c16
  c8. c16 c16 c16
}

