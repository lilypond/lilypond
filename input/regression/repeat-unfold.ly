\version "2.19.21"

\header{
  texidoc="LilyPond has two modes for repeats: unfolded and semi-unfolded.
Unfolded repeats are fully written out.  Semi unfolded repeats have the body
written and all alternatives sequentially.  If the number of alternatives is
larger than the repeat count, the excess alternatives are ignored.  If the
number of alternatives is smaller, the first alternative is multiplied to
get to the number of repeats.

Unfolded behavior:"
}

#(ly:set-option 'warning-as-error #t)
%% One warning for the \repeat and one for the \alternative.
#(ly:expect-warning (G_ "More alternatives than repeats.  Junking excess alternatives"))
#(ly:expect-warning (G_ "More alternatives than repeats.  Junking excess alternatives"))

\context Voice \relative {
  \repeat unfold 3 { c''^"3x 0a" d }
  %% less alts than body
  \repeat unfold 4 { c^"4x 0a" d } \alternative { e f }
  %% more alts than body
  \repeat unfold 2 { c^"2x 3a" d } \alternative { e f g }
}
