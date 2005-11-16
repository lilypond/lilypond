\header {

  texidoc = "By setting @code{pagetopspace,} the Y position of the
first system can be forced to be uniform."

}
\version "2.7.13"

#(set-default-paper-size "a6")

\book {
  \score {

    \relative {
      c1\break\pageBreak
      c1\break\pageBreak
      c1
      \break\pageBreak
      \override TextScript #'padding = #20
      c1^"bla"
    }
  }

  \paper {
    pagetopspace = 3 \cm
  }
}

