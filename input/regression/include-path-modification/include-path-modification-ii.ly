\version "2.25.22"

\header {
  texidoc = "@code{ly:parser-append-to-include-path} appends to the include path
for the current parser only.  Files @file{include-path-modification-i.ly} and
@file{include-path-modification-ii.ly} include incompatible versions of
@file{include-test.ily}, yet they can be processed in a single
@command{lilypond} process without interfering with each other.

The expected output of this test is the markup @emph{v2.}"
}

#(ly:parser-append-to-include-path
  (string-append
   (dirname (current-filename))
   file-name-separator-string
   "mock-library-v2"))
\include "include-test.ily"
\testMarkupII
