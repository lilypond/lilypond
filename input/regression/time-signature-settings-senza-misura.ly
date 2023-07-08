\version "2.25.7"

\header {
  texidoc = "@code{\\overrideTimeSignatureSettings} can set beaming properties
to use when @code{timeSignatureFraction} is @code{#f}.  The output should have
no time signature or bar lines.  The first 15 notes should be grouped
@code{1,2,3,4,5}."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  forbidBreakBetweenBarLines = ##f
  \overrideTimeSignatureSettings
    ##f       % timeSignatureFraction
    1/8       % baseMomentFraction
    1,2,3,4,5 % beatStructure
    #'()      % beamExceptions
  timeSignatureFraction = ##f
} {
  \repeat unfold 30 c'8
}
