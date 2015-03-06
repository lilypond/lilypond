\version "2.19.16"

\header {
  texidoc = "The property @code{minimum-length-after-break} can be
used to stretch broken spanners starting after a line break.  The
following example shows usage for a variety of spanners.
"
}

\layout {
  ragged-right = ##t
}

{
  \once \override Tie.minimum-length-after-break = 20
  a1~
  \break
  a1

  \once \override Slur.minimum-length-after-break = 20
  a1(
  \break
  d'1)

  \once \override TextSpanner.springs-and-rods = #ly:spanner::set-spacing-rods
  \once \override TextSpanner.minimum-length-after-break = 20
  a1\startTextSpan
  \break
  a1\stopTextSpan

  \once \override Hairpin.after-line-breaking = ##t
  \once \override Hairpin.to-barline = ##f
  \once \override Hairpin.minimum-length-after-break = 20
  a1\<
  \break
  a1\!

  \once \override Glissando.springs-and-rods = #ly:spanner::set-spacing-rods
  \once \override Glissando.breakable = ##t
  \once \override Glissando.after-line-breaking = ##t
  \once \override Glissando.minimum-length-after-break = 20
  a1\glissando
  \break
  d'1
}
