\version "2.13.60"

\header {

  texidoc = "Links to labels should not break if the label doesn't exist."

}

\book {
  \markup { \with-link #'dummy \concat { "Link to non-existing label"  } }
}