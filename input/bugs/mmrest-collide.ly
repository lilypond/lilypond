%
% multi-measure-rests should collide with notes, just as normal rests.
%
\score {
  \notes\context Staff <
     \context Voice=one \relative c''{
       d d d d
       d d d d
     } 
     \context Voice=two {
       r1
       R
     } 
  >
}