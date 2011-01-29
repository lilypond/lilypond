\header {

  texidoc = "By setting @code{page-top-space,} the Y position of the
first system can be forced to be uniform."

}
\version "2.13.46"

#(set-default-paper-size "a6")

\book {
  \score {

    \relative c' {
      c1\break\pageBreak
      c1\break\pageBreak
      c1
      \break\pageBreak
      \override TextScript #'padding = #20
      c1^"bla"
    }
  }

  \paper {
    obsolete-page-top-space = 3 \cm
    top-system-spacing #'basic-distance = #(/ obsolete-page-top-space staff-space)
  }
}

