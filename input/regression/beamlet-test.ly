\version "2.15.20"

\header {
  texidoc = "
Beamlets should point away from complete beat units and toward off-beat or
broken beat units.  This should work in tuplets as well as in ordinary time.
"
}

\relative c'' {
    \times 2/3 {
      c8. c16 c8
    }
   \times 2/3 {
      c8 c16 c8.
    }
  \times 4/5 {
    c8[ c8. c16 c8 c8]
  }
  \times 4/5 {
    c8[ c8 c16 c8. c8]
  }
  \times 4/5 {
    c8 c16 c8. c8 c8
  }
  \times 4/5 {
    c8 c8 c8. c16 c8
  }
  c8.[ c16 c8 c8]
  c8[ c16 c8. c8]
  c8[ c8. c16 c8]
  c8.[ c16 c8. c16]
  \times 4/5 { c8 [ c16 c8 c16 c8 c8 ] }
  \times 4/5 { a8 a32 a8 a16. a8 a8 }
}

