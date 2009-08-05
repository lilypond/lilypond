\version "2.13.4"

\header {
  lsrtags = "rhythms"
  texidoc = "
Beaming patterns may be altered with the @code{beatGrouping} property:

"
  doctitle = "Grouping beats"
}

\relative c'' {
  \time 5/16
  \overrideBeamSettings #'Score #'(5 . 16) #'end
    #'((* . (2 3)))
  c8^"(2+3)" c16 c8
  \overrideBeamSettings #'Score #'(5 . 16) #'end
    #'((* . (3 2)))
  c8^"(3+2)" c16 c8
}
