\header
{
    texidoc = "  The handling of stems for harmonic notes must be
   completely identical to normal note heads."
}

\version "2.1.9"

\score {
  \context Voice \notes {
    << { c''4 }
       \new Thread {
	   \property Thread.NoteHead \set #'style = #'harmonic
	   f''4
       }
   >>
  }

  \paper {
    linewidth = 50.0\mm
    indent = 0.0\mm
  }
}
