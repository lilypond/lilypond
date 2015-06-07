\version "2.19.21"

\header {
  texidoc = "
Beamlets should point away from complete beat units and toward off-beat or
broken beat units.  This should work in tuplets as well as in ordinary time.
"
}

\relative {
    \tuplet 3/2 {
      c''8. c16 c8
    }
   \tuplet 3/2 {
      c8 c16 c8.
    }
  \tuplet 5/4 {
    c8[ c8. c16 c8 c8]
  }
  \tuplet 5/4 {
    c8[ c8 c16 c8. c8]
  }
  \tuplet 5/4 {
    c8 c16 c8. c8 c8
  }
  \tuplet 5/4 {
    c8 c8 c8. c16 c8
  }
  c8.[ c16 c8 c8]
  c8[ c16 c8. c8]
  c8[ c8. c16 c8]
  c8.[ c16 c8. c16]
  \tuplet 5/4 { c8 [ c16 c8 c16 c8 c8 ] }
  \tuplet 5/4 { a8 a32 a8 a16. a8 a8 }
}

