\version "2.27.5"

\header {
  texidoc = "The @code{\\fill-line} markup command should align texts in
columns.  For example, the characters in the center should form one column.

Also test behaviour if entries are too long; there shouldn't be any
collisions - at the cost of lengthening the line, probably running out of page.

Apart from first and last entry, empty stencil are removed, but not point
stencils."
}
%% The default `bookTitleMarkup` has two lines with multiple entries:
%%      poet instrument composer
%%      meter           arranger
%% If one of those entries is unset it results in an empty stencil.
%% We need to respect them at line start and end, otherwise positioning of other
%% entries of that line is disturbed.
%% If `instrument` is empty we remove the empty stencil in order give `composer`
%% the possibility to extend more to the left.

#(set-default-paper-size "a5landscape")

\paper {
  bookTitleMarkup = \markup {
    \column {
      \fill-line {
        \fromproperty #'header:poet
        \fromproperty #'header:instrument
        \fromproperty #'header:composer
      }
      \fill-line { 1 }
      \fill-line { 1 2 }
      \fill-line { 1 2 3 }
      \fill-line { 1 2 3 4 }
      \fill-line { 1 2 3 4 5 }
      \fill-line { 1 2 3 4 5 6 }
      \fill-line { 1 2 3 4 5 6 7 }
      \fill-line { 1 2 3 4 5 6 7 8 }
      \fill-line { 1 2 3 4 5 6 7 8 9 }
      \fill-line { aaa bbbb ccccc dddd eee ffff ggggg hhhh iii }

      \vspace #1

      \fill-line { 11111111111 2 3 4 5 6 7 8 9 }
      \fill-line { 1 2 3 4 5 6 7 8 99999999999 }
      \fill-line { 1 2 3 44444444444 55555555555 66666666666 7 8 9 }
      \fill-line {
        1111111111
        2222222222
        3333333333
        4444444444
        5555555555
        6666666666
        7777777777
        8888888888
        9999999999
      }

      \vspace #1

      \fill-line { | | | | | | | | | }
      \fill-line { "" | | | | | | | "" }
      \fill-line { "" | | \null | \null | | "" }
      \fill-line { "" | | "" | "" | | "" }
    }
  }

  tagline = ##f
}

\header {
  poet = ##f
  instrument = ##f
  composer = "composer-composer-composer-composer-composer-composer-composer"
}

\book {
  \score {
    \new Staff \relative c'' {
      \*4 c1
    }
  }
}
