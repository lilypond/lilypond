
\header {

  texidoc = " A dotted whole note displayed via the @code{\\note}
  command must separate the note head and the dot. The dot avoids the upflag."

	}
\paper {
  packed = ##T
}

\version "2.10.10"


\relative {
  c^\markup { \note #"1." #1 }
  c^\markup { \note #"2." #1 }
  c^\markup { \note #"8." #1 }
}

