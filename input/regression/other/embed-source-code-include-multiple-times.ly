\version "2.25.3"

\header {
  texidoc = "With the @option{-dembed-source-code} option,
a file included multiple times is only embedded once.

Note: with the current regression testing infrastructure,
any breakage of this test will not be noticed in
@code{make check}."
}

#(ly:set-option 'embed-source-code)

\include "included.ily"
\include "included.ily"

{ c' }
