\version "2.25.26"

\header {
  texidoc = "Top-level unspecified music expressed literally with @code{$#@{#@}}
or referenced by a variable is discarded.  This test should show just the
message, ``There are no scores.''"
}

#(ly:set-option 'warning-as-error #t)

testMusic = $#{#}

\book {
  \markup "There are no scores."

  $#{#}
  \testMusic

  \bookpart {
    $#{#}
    \testMusic
  }
}

$#{#}
\testMusic
