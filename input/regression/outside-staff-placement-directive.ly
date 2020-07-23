\version "2.17.6"

\header {
  texidoc = "The @code{outside-staff-placement-directive} adjusts
  the order in which objects are placed outside the staff."
}

music = {
  f'2^"Black A,"
  2^"White E,"
  2^"Red I,"
  2^"Green U,"
  2^"Blue O:"
  2^"voyels:"
  2^"Rimbaud,"
  2^"“Voyels”"
}

{
  \override Staff.VerticalAxisGroup.outside-staff-placement-directive =
    #'left-to-right-polite
    \tempo left-to-right-polite \music }
{
  \override Staff.VerticalAxisGroup.outside-staff-placement-directive =
    #'left-to-right-greedy
    \tempo left-to-right-greedy \music }
{
  \override Staff.VerticalAxisGroup.outside-staff-placement-directive =
    #'right-to-left-polite
    \tempo right-to-left-polite \music }
{
  \override Staff.VerticalAxisGroup.outside-staff-placement-directive =
    #'right-to-left-greedy
    \tempo right-to-left-greedy \music }
