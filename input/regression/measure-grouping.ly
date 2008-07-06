
\header
{

  texidoc = "The @code{Measure_grouping_engraver} adds triangles and
brackets above beats when the beats of a time signature are grouped.  "

}

\version "2.11.51"

\layout  {
  ragged-right = ##t
}

%% TODO: should have 2/4 + 5/8 time sig style.

\context Staff \with {
    \consists "Measure_grouping_engraver"
  }

\relative c' {
  #(set-time-signature 2 4) 
  c8 a'4 a8~
  #(set-time-signature 5 8 '(3 2)) 
  a8 bes4 r8 bes8->
  \time 2/4
  c,8 g'4 g8~
  #(set-time-signature 5 8 '(3 2)) 
  g8 a4 g a4.->
}


