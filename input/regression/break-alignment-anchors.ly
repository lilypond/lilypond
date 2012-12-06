\version "2.17.6"

\header {
  texidoc = "The break-align-anchor property of a break-aligned grob gives
the horizontal offset at which other grobs should attach."
}

{
  \override Score.RehearsalMark.break-align-symbols = #'(staff-bar)
  c'1
  \once \override Staff.BarLine.break-align-anchor = #-5
  \mark \default
  \noBreak
  c'1
  \once \override Staff.BarLine.break-align-anchor = #5
  \mark \default
  \noBreak
  c'1
}
