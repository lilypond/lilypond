\version "2.23.6"

\header {
  texidoc = "@code{first-bar-number-invisible-and-no-parenthesized-bar-numbers}
is a bar number visibility where bar numbers are printed except for the first,
and except for broken measures."
}

\include "bar-number-visibility.ily"

\testVisibility #first-bar-number-invisible-and-no-parenthesized-bar-numbers
