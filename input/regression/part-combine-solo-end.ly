\header {

    texidoc = "SOLO is printed even if the solo voice ends before the
    other one. Unfortunately, the multi-rest of the 1st voice (which
    is 2 bars longer than the 2nd voice) does not get printed."

}

\version "2.11.51"
\layout { ragged-right = ##t }

<<
    \new Staff
    \partcombine
    { R1 * 2 }
    { c'8\> c'\! r2.  }
>>
