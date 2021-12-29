\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Customizing @code{measureBarType} is effective when
appropriate bar lines are defined.  The first system should end with a
single thick bar line with a dashed span.  The second system should
end with a single thick bar line spanning the whole system."
}

\layout {
  ragged-right = ##t
}

\defineBarLine ".-test" #'("." #f "!")
\defineBarLine "S-.-test" #'(".-test" "S" "=")

staff = \new Staff \fixed c' {
  R1 \break \inStaffSegno R1
}

piece = \new PianoStaff << \staff \staff >>

\new Score \with { measureBarType = ".-test" } << \piece >>
