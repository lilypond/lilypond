\header {

    texidoc = "SOLO is printed even if the solo voice ends before the
    other one."

}

\version "2.3.0"

\score {
   <<
     \new Staff
       \partcombine
         \notes { R1 * 2 }
         \notes { c'8\> c'\! r2.  }
   >>
}
