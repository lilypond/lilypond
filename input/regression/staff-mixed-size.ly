\header {
    
texidoc = "Staffs may be present in several sizes within a score.
This is achieved with an internal scaling factor. When code forgets
factor, objects generally become too thick or too large.
"

}
\version "2.1.23"

\score {
  <<
    \new Staff \with {
      fontSize = #-4
      \override StaffSymbol #'staff-space = #(magstep -4)
    }
    \notes \relative c' {
\override DynamicText  #'extra-offset = #'(0 . 3)
      s1-\f c''8[(\< r a g]) e[ r d( <f a>])\! \times 2/3 { d4 d d }
    }
    \new Staff
    \notes \relative c' {
\override DynamicText  #'extra-offset = #'(0 . 3)
      s1-\f c''8[(\< r a g]) e[ r d( <f a>])\! \times 2/3 { d4 d d }
    }
  >>

  \paper {
    #(paper-set-staff-size 6)
    raggedright = ##t
  }
}
