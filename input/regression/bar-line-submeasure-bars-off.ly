\version "2.25.32"

\header {
  texidoc="@code{\\submeasureBarsOn} enables engraving of submeasure bar lines,
and @code{\\submeasureBarsOff} disables it."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 0
}

testMusic = \fixed c' {
  << { c'2 2 } \\ { a2 2 } >> |
  r2 r |
  R1  |
  << R1 \\ { a2 2 } >> |
}

{
  \time #'((1 . 2) (1 . 2))

  \sectionLabel "Default"
  \testMusic

  \section
  \sectionLabel "On"
  \submeasureBarsOn
  \testMusic

  \section
  \sectionLabel "Off"
  \submeasureBarsOff
  \testMusic
}
