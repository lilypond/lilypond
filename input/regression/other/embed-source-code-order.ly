\version "2.25.3"

\header {
  texidoc = "When using the @option{-dembed-source-code} option, embedded
files are displayed in alphabetical order.

Note: with the current regression testing infrastructure,
any breakage of this test will not be noticed in
@code{make check}."
}

#(ly:set-option 'embed-source-code)

% files will be reordered
\include "included-a.ily"
\include "included-g.ily"
\include "included-b.ily"
\include "included-h.ily"
\include "included-d.ily"
\include "included-k.ily"
\include "included-e.ily"
\include "included-f.ily"
\include "included-i.ily"
\include "included-c.ily"
\include "included-j.ily"

{ c' }
