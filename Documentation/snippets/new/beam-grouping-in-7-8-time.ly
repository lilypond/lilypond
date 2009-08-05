\version "2.13.4"

\header {
  lsrtags = "rhythms"
  texidoc = "
There are no default automatic beam groupings specified for 7/8 time,
so if automatic beams are required the grouping must be specified.  For
example, to group all beams 2-3-2 in 7/8 time, specify beam endings at
2/8 and 5/8:
"
  doctitle = "Beam grouping in 7/8 time"
}

\relative c'' {
  \time 7/8
  % rhythm 2-3-2
  a8 a a a a a a
  \overrideBeamSettings #'Score #'(7 . 8) #'end
    #'((* . (2 3 2)))
  a8 a a a a a a
}
