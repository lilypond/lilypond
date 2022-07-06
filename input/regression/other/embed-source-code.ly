\version "2.23.11"

\header {
  texidoc = "@code{-dembed-source-code} embeds source files
in PDFs, including include files, images and verbatim files.

Note: with the current regression testing infrastructure,
any breakage of this test will not be noticed in
@code{make check}."
}

#(ly:set-option 'embed-source-code)

\include "included.ily"

{ \someMusic }

\markup \epsfile #X #20 "lilypond.eps"

\markup \verbatim-file "verbatim"
