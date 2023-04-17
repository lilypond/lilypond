\version "2.25.4"

\header {
  texidoc = "There is no error when a file contains
TOC items but no TOC."
}

% This used to error out in the Cairo backend.
\tocItem \markup foo
{ c' }
