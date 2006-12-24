
\header
{

  texidoc = "The @code{Measure_grouping_engraver} adds triangles and
brackets above beats when the beats of a time signature are grouped.  "

}

\version "2.10.0"


%% TODO: should have 2/4 + 5/8 time sig style.

\context Staff
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
\layout  {
  ragged-right = ##t
  \context {
    \Staff
    \consists "Measure_grouping_engraver"
  }
}


