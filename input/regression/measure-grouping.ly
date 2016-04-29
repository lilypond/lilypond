
\header
{

  texidoc = "The @code{Measure_grouping_engraver} adds triangles and
brackets above beats when the beats of a time signature are grouped.  "

}

\version "2.19.40"

\layout  {
  ragged-right = ##t
}

%% TODO: should have 2/4 + 5/8 time sig style.

\context Staff \with {
    \consists "Measure_grouping_engraver"
  }

\relative {
  \time 2/4 
  c'8 a'4 a8~
  \time 3,2 5/8 
  a8 bes4 r8 bes8->
  \time 2/4
  c,8 g'4 g8~
  \time 3,2 5/8 
  g8 a4 g a4.->
}


