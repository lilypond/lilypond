\header {

  texidoc = "Dotted rests connected with beams do not trigger
  premature beam calculations.  In this case, the beam should be
  sloped, and there should be no programming_error() warnings."

}

\version "2.19.21"
\new Staff \relative
{
  <<
    { \time 12/16 c''16[ b a r  b g] }
    \\
    { r8. r }
  >>
}


