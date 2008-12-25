
\header {
  texidoc = "Individual ties may be formatted manually by
specifying their @code{direction} and/or @code{staff-position}."
  
}

\version "2.12.0"

\paper {
  ragged-right = ##t
}

{
  \override Tie #'staff-position = #-5.5
  c'4 ~ c'
  \override Tie #'staff-position = #-6.5
  c'4 ~ c'
  \override Tie #'staff-position = #-7.5
  c'4 ~ c'
  \revert Tie #'staff-position
  \override Tie #'direction = #UP
  c'4 ~ c'
}
