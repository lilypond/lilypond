\header {

    texidoc = "SOLO is printed even if the solo voice ends before the
    other one. Unfortunately, the multi-rest does not get printed."
}

\version "2.3.17"
\paper { raggedright = ##t }

<<
    \new Staff
    \partcombine
    { R1 * 2 }
    { c'8\> c'\! r2.  }
>>
