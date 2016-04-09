\version "2.19.40"

\header {
  texidoc = "
Multiple overrides to the default time signature settings can be
added.  In this example, notes should be beamed as indicated
by the markups.
"
}

\new Staff{
  \relative c' {
    \overrideTimeSignatureSettings
      #'(4 . 4)  % time signature fraction
      #'(1 . 4)  % base moment fraction
      1,1,1,1        % beatStructure
      #'()       % beamExceptions
    \overrideTimeSignatureSettings
      #'(3 . 4)  % time signature fraction
      #'(1 . 4)  % base moment fraction
      1,1,1        % beatStructure
      #'()       % beamExceptions
    \time 4/4
    c8^\markup {"Beam by 1/4"} c c c c c c c |
    \time 3/4
    c8^\markup {"Beam by 1/4"} c c c c c |
    \revertTimeSignatureSettings #'(4 . 4)
    \revertTimeSignatureSettings #'(3 . 4)
    \time 4/4
    c8^\markup {"Beam by 1/2"} c c c c c c c |
    \time 3/4
    c8^\markup {"Beam by 3/4"} c c c c c |
  }
}
