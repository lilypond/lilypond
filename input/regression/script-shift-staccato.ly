
\header {
  texidoc = "The horizontal placement of staccato dots above an
upstem or below a downstem note differs from the placement of
other scripts in that different positioning is used when the dot is
alone and when it is part of a compound articulation.  The
property @code{toward-stem-shift-in-column} ensures good default
positioning of the staccato (see first measure below), and allows
precise horizontal control of a column containing a staccato and
of the staccato within it (second measure).  (@code{0.0} means
centered on the note head, @code{1.0} means centered on the stem.)
"
}

\version "2.19.21"

\relative
{
  % default
  a'4^. c_.
  a^.^- c_._-

  \override Script.toward-stem-shift-in-column = 1.0
  a4^.^- c_._-
  \revert Script.toward-stem-shift-in-column

  a4-\tweak toward-stem-shift-in-column 1.0 ^. ^-
  c4-\tweak toward-stem-shift-in-column 1.0 _. _-
}
