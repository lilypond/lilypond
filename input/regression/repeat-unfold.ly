\version "1.5.68"

\header{
texidoc="
LilyPond has three modes for repeats: folded, unfolded and
semi-unfolded.  Unfolded repeats are fully written out. Semi unfolded
repeats have the body written and all alternatives sequentially.
Folded repeats have the body written and all alternatives
simultaneo.ly.  If the number of alternatives is larger than the
repeat count, the excess alternatives are ignored.  If the number of
alternatives is smaller, the first alternative is multiplied to get to
the number of repeats.

Unfolded behavior:
"
}

\score {

  \context Voice \notes\relative c'' {
\repeat unfold 3 { c^"3x 0a" d }
% less alts than body
\repeat unfold 4 { c^"4x 0a" d } \alternative { e f }
% more alts than body
\repeat unfold 2 { c^"2x 3a" d } \alternative { e f g } 
}}

