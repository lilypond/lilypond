\version "2.23.11"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Customizing @code{sectionBarType} is effective when
appropriate bar lines are defined.  The system should end with a
double bar line with a thick span."
}

\layout {
  ragged-right = ##t
}

\defineBarLine "||-test" #'("||-test" #f ".")

staff = \new Staff \fixed c' {
  R1 \section
}

piece = \new PianoStaff << \staff \staff >>

\new Score \with { sectionBarType = "||-test" } << \piece >>
