\version "2.27.5"

\header {
  texidoc = "@code{\\rigid} stops the move-before-grace functionality of its
music argument"
}

\layout {
  indent = 4\cm
}

\new StaffGroup \with { instrumentName = \markup {using \typewriter "\\rigid" } }
{
  <<
    { \rigid \bar "||" c'1 }
    { \grace g'4 c''1 }
  >>
}

\new StaffGroup \with { instrumentName = "standard" }
{
  <<
    { \bar "||" c'1 }
    { \grace g'4 c''1 }
  >>
}
