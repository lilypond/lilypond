\version "2.19.22"

\header {
texidoc = "Postfix functions for custom crescendo text spanners.  The spanners
should start on the first note of the measure.  One has to use -\mycresc,
otherwise the spanner start will rather be assigned to the next note."
}

% Two functions for (de)crescendo spanners where you can explicitly give the
% spanner text.
mycresc = #(define-music-function (mymarkup) (string?)
  (make-music 'CrescendoEvent 'span-direction START
              'span-type 'text 'span-text mymarkup))
mydecresc = #(define-music-function (mymarkup) (string?)
  (make-music 'DecrescendoEvent 'span-direction START
              'span-type 'text 'span-text mymarkup))

\relative {
  c'4-\mycresc "custom cresc" c4 c4 c4 |
  c4 c4 c4 c4 |
  c4-\mydecresc "custom decresc" c4 c4 c4 |
  c4 c4\! c4 c4
}



