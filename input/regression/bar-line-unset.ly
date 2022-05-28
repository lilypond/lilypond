\version "2.23.10"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Automatic bar types that are set to @code{'()} or are unset
are ignored, allowing lower-priority bar types to appear.  In this
case, there should be no line breaks and a single thick bar line
should appear at the end under a segno."
}

\new Score \with {
  \unset endRepeatBarType
  measureBarType = #'()
  sectionBarType = #'()
  underlyingRepeatBarType = "." % distinguish from default sectionBarType
} {
  \repeat volta 2 { \repeat unfold 12 R1 } % endRepeatBarType
  \section % sectionBarType
  \segnoMark 1 % underlyingRepeatBarType
}
