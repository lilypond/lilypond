\version "2.23.6"

\header {
  texidoc = "@code{all-bar-numbers-visible} is a bar number visibility
where all bar numbers are printed, including bar numbers for the
first measure and for broken measures."
}

\include "bar-number-visibility.ily"

\testVisibility#all-bar-numbers-visible
