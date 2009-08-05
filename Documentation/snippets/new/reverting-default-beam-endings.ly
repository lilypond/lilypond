\version "2.13.4"

\header {
  lsrtags = "rhythms"
  texidoc = "
To typeset beams grouped @code{3-4-3-2} in 12/8 it is no longer
necessary first to override the default beam endings in 12/8,and
then to set up the new beaming endings:
"
  doctitle = "Reverting default beam endings"
}

\relative c'' {
  \time 12/8

  % Default beaming
  a8 a a a a a a a a a a a

  % Set new values for beam endings
  \overrideBeamSettings #'Score #'(12 . 8) #'end
    #'((* . (3 4 3 2)))
  a8 a a a a a a a a a a a
}
