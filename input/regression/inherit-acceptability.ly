\version "2.19.26"

\header {
  texidoc = "@code{\\inherit-acceptability} allows for one context def to be accepted
wherever an existing one is."
}

\new StaffGroup
<<
  \new Staff { \time 3/4 c'2. }
  \new TimeLess { \time 3/4 c'2. }
>>

\layout {
  ragged-right = ##t
  \context {
    \Staff
    \name "TimeLess"
    \remove "Time_signature_engraver"
  }
  \inherit-acceptability "TimeLess" "Staff"
}
