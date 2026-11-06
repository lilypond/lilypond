\version "2.25.35"

\header {
  texidoc = "@code{\\overrideTimeSignatureSettings} can set beaming properties
to use when @code{timeSignature} is @code{#f}.  The output should have
no time signature or bar lines.  The first six notes should be grouped
@code{1,2,3}, and that grouping should repeat thrice.  In the second section,
the grouping @code{4,2} should repeat thrice."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
  ragged-right = ##t
}

\new Score \with {
  forbidBreakBetweenBarLines = ##f
  \overrideTimeSignatureSettings
    ##f       % timeSignature
    #1/8      % beatBase
    1,2,3     % beatStructure
    \beamExceptions { 16[ 16 16 16] 16[ 16] }
  timeSignature = ##f
} {
  \*18 c'8 \section
  \*18 c'16
}
