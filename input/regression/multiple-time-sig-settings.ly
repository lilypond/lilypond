\version "2.13.33"

\header {
  texidoc = "
Multiple overrides to the default time signature settings can be
added.  In this example, all notes should be beamed at 1/4.
"
}

\relative c' {
  \overrideTimeSignatureSettings
      #'Score
      #'(4 . 4)  % time signature fraction
      #'(1 . 4)  % base moment fraction
      #'(1 1 1 1)    % beatStructure
      #'()       % beamExceptions
  \overrideTimeSignatureSettings
      #'Score
      #'(3 . 4)  % time signature fraction
      #'(1 . 4)  % base moment fraction
      #'(1 1 1)    % beatStructure
      #'()       % beamExceptions
  \time 4/4
  c8 c c c c c c c |
  \time 3/4
  c8 c c c c c |
  \time 4/4
  c8 c c c c c c c |
  \time 3/4
  c8 c c c c c |
}
