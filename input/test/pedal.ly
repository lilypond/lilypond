\header{
texidoc = "Piano pedal symbols merge stop and start.  The strings are configurable. ";
}

\version "1.3.120";


\score{
\context Staff \notes\relative c'{
c4\sustainDown d e f\sustainUp g\sustainDown b c
c, [d16 \sustainUp \sustainDown c  c c] [e e \sustainUp \sustainDown e e ] f4 \sustainUp g\sustainDown b c
\property Staff.pedalSustainStrings = #'("-" "-P" "P")
\property Staff.SustainPedal \override #'padding = #2
c, \sustainUp\sustainDown d e f
 \sustainUp g\sustainDown b c
}
\paper{
}
\midi{
\tempo 4 = 60;
}
}
