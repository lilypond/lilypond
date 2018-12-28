\header {

    texidoc = "SOLO is printed even if the solo voice ends before the
    other one. Unfortunately, the multi-rest of the 1st voice (which
    is 2 bars longer than the 2nd voice) does not get printed."

}

\version "2.21.0"
\layout { ragged-right = ##t }

<<
    \new Staff
    \partCombine
    { R1 * 2 }
    { c'8\> c'\! r2.  }
>>
