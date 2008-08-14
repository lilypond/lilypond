\version "2.11.51"

\header {

  texidoc = "System separators may be defined as markups in the
@code{systemSeparator} field of the paper block.  They are centered
between the boundary staves of each system."

}

\paper {
  systemSeparatorMarkup = \slashSeparator

}
foobar = \relative { c1 c \break c c  \break c c }
\book
{
  \score {
     \new GrandStaff <<
       \new Staff \foobar 
       \new Staff \foobar 
     >>
  }
}
