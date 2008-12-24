\header {
  texidoc = "
	In packed mode, pack notes as tight as possible.  This makes
	sense mostly in combination with ragged-right mode: the notes
	are then printed at minimum distance.  This is mostly useful
	for ancient notation, but may also be useful for some flavours
	of contemporary music.  If not in ragged-right mode, lily will
	pack as many bars of music as possible into a line, but the
	line will then be stretched to fill the whole linewidth.
"
  }

\version "2.12.0"

\relative {
  \override Score.SpacingSpanner #'packed-spacing = ##t
  c2 d4 f8[ g] a
}
