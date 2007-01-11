\header {
  
  texidoc = "Staves may be present in several sizes within a score.
This is achieved with an internal scaling factor. If the scaling factor is 
forgotten in some places, objects generally become too thick or too 
large on smaller staves."

}

\version "2.11.11"

\layout {
  #(layout-set-staff-size 6)
  ragged-right = ##t
}

<<
  \new Staff \with {
    fontSize = #-4
    \override StaffSymbol #'staff-space = #(magstep -4)
  }
  \relative c' {
    \override DynamicText  #'extra-offset = #'(0 . 3)
    s1-\f c''8[(\< r a g]) e[ r d( <f a>])\! \times 2/3 { d4 d d }
			    }
    \new Staff
    \relative c' {
      \override DynamicText  #'extra-offset = #'(0 . 3)
      s1-\f c''8[(\< r a g]) e[ r d( <f a>])\! \times 2/3 { d4 d d }
			      }
    >>


