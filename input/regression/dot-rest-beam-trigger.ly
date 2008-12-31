\header {

  texidoc = "Dotted rests connected with beams do not trigger
  premature beam calculations.  In this case, the beam should be
  sloped, and there should be no programming_error() warnings."

}

\version "2.12.0"
\new Staff \relative c''
{
  <<
    { \time 12/16 c16[ b a r  b g] }
    \\
    { r8. r }
  >>
}


