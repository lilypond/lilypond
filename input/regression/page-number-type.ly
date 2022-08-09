\version "2.23.12"

\header {
  texidoc = "Test the different options for page number formatting."
}

#(set-default-paper-size "a9")

\paper {
  print-first-page-number = ##t
  bookpart-level-page-numbering = ##t
}

\book {
  \bookpart {
    \paper {
      page-number-type = #'roman-lower
    }
    { c'1 \pageBreak c'1 }
  }
  \bookpart {
    \paper {
      page-number-type = #'roman-upper
    }
    { c'1 \pageBreak c'1 }
  }
  \bookpart {
    \paper {
      page-number-type = #'arabic % default
    }
    { c'1 \pageBreak c'1 }
  }
  \bookpart {
    \paper {
      page-number-type = #'roman-ij-lower
    }
    { c'1 \pageBreak c'1 }
  }
  \bookpart {
    \paper {
      page-number-type = #'roman-ij-upper
    }
    { c'1 \pageBreak c'1 }
  }
}
