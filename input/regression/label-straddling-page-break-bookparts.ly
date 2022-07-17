\version "2.23.11"

\header {
  texidoc = "When a label straddles at a page break, the chosen
page is the second one.  This also works when there are several
bookparts.

Note: you need to compile this regtest on its own to check it, as
the @command{lilypond-book} setup does not work for page references."
}

\markup \column {
  \line { Should be 3: \page-ref #'labI "  " "??" }
  \line { Should be 5: \page-ref #'labII "  " "??" }
}

\bookpart {
  {
    c'1
    \pageBreak
    \label labI
    c'1
  }
}

\bookpart {
  {
    c'1
    \pageBreak
    \label labII
    c'1
  }
}
