\version "2.19.45"

\header {
  texidoc="When there is a gap between the end of a crescendo and a
  subsequent explicit dynamic, the dynamic performer uses the explicit
  dynamic as the target of the crescendo."

  %% Note: Choosing this behavior simplified the implementation.  In
  %% the developer's opinion, it is difficult to argue that choosing a
  %% target dynamic that under- or overshoots the explicit dynamic is
  %% more correct.

}

\score {
   { c\mf\< c\! c\f }
   \midi {}
}
