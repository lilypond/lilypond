
\version "2.1.7"
% TODO bit too wordy; edit a bit to cut stuff.  -gp
\header { texidoc ="@cindex Slur Beautiful
Similarly, the curvature of a slur is adjusted to stay clear of note
heads and stems.  When that would increase the curvature too much, the
slur is reverted to its default shape.  The threshold for this
decision is in Slur's object property @code{beautiful}.
It is loosely related to the enclosed area between the slur and the
notes.  Usually, the default setting works well, but in some cases you
may prefer a curved slur when LilyPond decides for a vertically moved
one.  You can indicate this preference by increasing the
@code{beautiful} value."
}

\score { \notes {\relative c' {
  \stemDown \slurUp
  c16( a' f' a a f a, c,)
  c( a' f' a a f d, c)
  \property Voice.Slur \override #'beautiful = #5.0
  c( a' f' a a f d, c)
}}
\paper { raggedright = ##t }
}
