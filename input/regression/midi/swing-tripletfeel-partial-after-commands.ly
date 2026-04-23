\version "2.27.1"

\header {
  texidoc = "@code{\\tripletFeel} detects @code{\\partial} correctly even
when other commands such as @code{\\clef}, @code{\\time}, or @code{\\key}
precede it."
}

#(ly:set-option 'warning-as-error #t)

\include "swing.ly"

%% Music variables are used so that \relative is resolved at definition time,
%% giving plain SequentialMusic that extract-partial-offset can walk.

musicClefPartial = {
  \clef alto
  \partial 8
  c'8 | c'8
}

musicTimeKeyPartial = {
  \time 4/4
  \key d \major
  \partial 4
  d'8 d'8 |
}

\score {
  \new Staff \tripletFeel 8 \musicClefPartial
  \midi { }
}

\score {
  \new Staff \tripletFeel 8 \musicTimeKeyPartial
  \midi { }
}
