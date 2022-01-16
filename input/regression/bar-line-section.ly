\version "2.23.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="@code{\\section} creates a section bar line whether or not
it is aligned on a measure boundary, except at the start of the piece.
This test should show a double bar line after each of the three notes."
}

\layout {
  ragged-right = ##t
}

staff = \new Staff \fixed c' {
  \section %% ignored at beginning of piece
  e2 \section f2 \section |
  g1 \section |
}

\new PianoStaff << \staff \staff >>
