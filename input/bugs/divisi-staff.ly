%
% space after barline on 1st staff due to time sig.
%

\score{
  \notes\relative c'\context GrandStaff{

c4 d e f | 
g a b c | \break

<{
d c b a | 
  g f e d}
 \context Staff=lower{
% \property Staff.TimeSignature = \turnOff

b' a g f |
  e d c b |}> \break

c1
}
\paper{
  \translator{
    \GrandStaffContext
    \consists "Instrument_name_engraver"
  }
}
}
