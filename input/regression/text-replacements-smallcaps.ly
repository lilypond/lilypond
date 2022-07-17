\version "2.23.12"

\header {
  texidoc = "When @code{\\smallCaps} and text replacements are
used together, the result of text replacements is also written
in small caps."
}

%% Both of these should give the same output.
\markup \replace #'(("2nd" . "second")) \smallCaps "2nd time"
\markup \smallCaps \replace #'(("2nd" . "second")) "2nd time"
