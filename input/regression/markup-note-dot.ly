\version "2.16.0"
\header {

  texidoc = " A dotted whole note displayed via the @code{\\note}
  command must separate the note head and the dot. The dot avoids the upflag."

	}

\relative c' {
  c4^\markup { \note #"1." #1 }
  c4^\markup { \note #"2." #1 }
  c4^\markup { \note #"8." #1 }
}
