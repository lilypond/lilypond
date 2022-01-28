\version "2.23.6"

\header {
  texidoc = "@code{first-bar-number-invisible} is a bar number
visibility where all bar numbers can be printed, including for
broken bars, except for the first measure, not even when broken."
}

\include "bar-number-visibility.ily"

\testVisibility #first-bar-number-invisible