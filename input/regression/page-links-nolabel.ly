\version "2.16.0"

\header {

  texidoc = "Links to labels should not break if the label doesn't exist."

}

\book {
  \markup { \with-link #'dummy \concat { "Link to non-existing label"  } }
}
