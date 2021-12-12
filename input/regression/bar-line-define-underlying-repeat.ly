\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Customizing @code{underlyingRepeatBarType} is effective when
appropriate bar lines are defined.  The first system should end with a
single thick bar line with a dashed span."
}

\layout {
  ragged-right = ##t
}

\defineBarLine ".-test" #'(".-don't-care" #f "!")
\defineBarLine "S-.-test" #'(".-test" "S" "=")

staff = \new Staff \fixed c' {
  r2. \break \inStaffSegno r4
}

piece = \new PianoStaff << \staff \staff >>

\new Score \with { underlyingRepeatBarType = ".-test" } << \piece >>
