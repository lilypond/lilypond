% from José Luis Cruz <joseluis AT jazzartistas.com>
\version "2.1.0"

\header { 
  texidoc="@cindex A tie in RhythmicStaff fails to compile."
}

\score { 
  \context RhythmicStaff {
    \notes { 
        c1 ~ c1
    }
  }
}
