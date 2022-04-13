\version "2.23.8"

\header {
  texidoc = "Default values for time signature settings can vary by
staff if the @code{Timing_translator} is moved from @code{Score} to
@code{Staff}.  In this case, the upper staff should be beamed 3/4,
1/4.  The lower staff should be beamed 1/4, 3/4."
}

\score {
  \new StaffGroup <<
     \new Staff {
        \overrideTimeSignatureSettings
          4/4        % timeSignatureFraction
          1/4        % baseMomentFraction
          3,1        % beatStructure
          #'()       % beamExceptions
        \time 4/4
        \repeat unfold 8 {c''8}
     }
     \new Staff {
        \overrideTimeSignatureSettings
          4/4        % timeSignatureFraction
          1/4        % baseMomentFraction
          1,3        % beatStructure
          #'()       % beamExceptions
        \time 4/4
        \repeat unfold 8 {c''8}
     }
  >>
  \layout {
    \context {
      \Score
      \remove "Timing_translator"
    }
    \context {
      \Staff
      \consists "Timing_translator"
    }
  }
}
