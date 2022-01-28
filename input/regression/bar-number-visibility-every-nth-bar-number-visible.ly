\version "2.23.6"

\header {
  texidoc = "@code{every-nth-bar-number-visible} is a bar number
visibility generator that prints bar numbers at regular intervals
of @math{n}: @math{n}, @math{2n}, etc."
}

\include "bar-number-visibility.ily"

\testVisibility #(every-nth-bar-number-visible 3)
