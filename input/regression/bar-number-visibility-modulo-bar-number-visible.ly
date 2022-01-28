\version "2.23.6"

\header {
  texidoc = "@code{modulo-bar-number-visible} is a bar number
visibility generator that generalizes @code{every-nth-bar-number-visible},
printing bar numbers at regular intervals of @var{n} that do not
necessarily start at @math{n}: @math{k}, @math{k + n}, @math{k + 2n},
etc."
}

\include "bar-number-visibility.ily"

\testVisibility #(modulo-bar-number-visible 3 2)
