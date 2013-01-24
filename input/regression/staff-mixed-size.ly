\header {

  texidoc = "Staves may be present in several sizes within a score.
This is achieved with an internal scaling factor. If the scaling factor is
forgotten in some places, objects generally become too thick or too
large on smaller staves."

}

\version "2.17.11"

\layout {
  ragged-right = ##t
}

melody = \relative c''' {
  \override DynamicText.extra-offset = #'(0 . 3)
  s1-\f c8[(\< r a g]) e[ r d( <f a>])\! \tuplet 3/2 { d4 d d }
}

<<
  \new Staff \with {
    fontSize = #-4
    \override StaffSymbol.staff-space = #(magstep -4)
  } {
    \melody
  }
  \new Staff {
    \melody
  }
>>
