\version "2.23.6"

\header {
  texidoc = "@code{first-bar-number-invisible-save-broken-bars} is
a bar number visibility that prints all bar numbers, including
for broken bars, except for an unbroken number of the first bar."
}

\include "bar-number-visibility.ily"

\testVisibility #first-bar-number-invisible-save-broken-bars
