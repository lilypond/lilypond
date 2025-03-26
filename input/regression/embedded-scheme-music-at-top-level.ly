\version "2.24.4"

\header {
  texidoc = "The return value of top-level embedded Scheme is discarded even
when it is music.  This test should show just the message, ``There are no
scores.''"
}

#(ly:set-option 'warning-as-error #t)

\book {
  \markup "There are no scores."

  ##{ #}
  ##{ c #}
  ##{ d e #}

  \bookpart {
    ##{ #}
    ##{ c #}
    ##{ d e #}
  }
}

##{ #}
##{ c #}
##{ d e #}
