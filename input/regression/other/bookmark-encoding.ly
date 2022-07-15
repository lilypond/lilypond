\version "2.23.11"

\header {
  texidoc = "PDF bookmarks can contain UTF-8 characters.  They
are correctly rendered in PDF viewers.

Note: due to the way the regression test infrastructure currently
works, any breakage in this test will not be noticed automatically."
}

\markuplist \table-of-contents
\tocItem \markup "(Łïłîρøñđ)"
{ c' }
