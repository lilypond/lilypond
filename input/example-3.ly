%%% A full example with two staves

\header {
  title = "And now, example 3"
  copyright = "public domain"
}

one = \relative {
  c' d e f
}

two = \relative {
  \clef "bass"
  c2 g2
}

<<
  \new Staff \one
  \new Staff \two
>>

%{
Type

    lilypond example-3
    xpdf example-3     # or your PDF viewer here

For learning LilyPond, please read the tutorial

   http://lilypond.org/tutorial

also included in the user-manual.
%}

%% Optional version number
\version "2.12.0"
