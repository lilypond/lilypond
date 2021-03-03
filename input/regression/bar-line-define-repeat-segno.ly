\version "2.23.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="User-defined bar lines with in-staff segni can be printed
by setting the @code{segnoType}, @code{startRepeatSegnoType},
@code{endRepeatSegnoType}, and @code{doubleRepeatSegnoType} context
properties.  The output should show two adjacent repeated sections
with unusually ornate bar lines with in-staff segni, followed by an
in-staff segno that is flanked by thick bar lines."
}

\layout {
  ragged-right = ##t
}

\defineBarLine "S.[|;:" #'("." "S.[|;:" "   |") % start repeat
\defineBarLine ":;|].S" #'(":;|]." "S" "  |   ") % end repeat
\defineBarLine ":;|]S[|;:" #'(":;|]." "S.[|;:" "  |   |") % double repeat
\defineBarLine ".S." #'("." ".S." " = ") % segno alone

\new Score \with {
  doubleRepeatSegnoType = ":;|]S[|;:"
  endRepeatSegnoType = ":;|].S"
  startRepeatSegnoType = "S.[|;:"
  segnoType = ".S."
} \new Staff \fixed c' {
  %% This test is intended to be sensitive to enhancements that would
  %% allow different behavior where bar lines are not measure-aligned.
  r2. \repeat unfold 2 { \repeat volta 2 { \inStaffSegno r4 | r2. } }
  \inStaffSegno r4 | r2. \inStaffSegno r4 |
}
