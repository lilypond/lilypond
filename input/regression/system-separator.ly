\version "2.7.39"

\header {

  texidoc = "System separators maybe defined as markups in the
@code{systemSeparator} field of the bookpaper block. They are centered
between the boundary staffs of each system. "

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
