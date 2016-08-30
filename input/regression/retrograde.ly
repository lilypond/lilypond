\version "2.18.0"

\header {
  texidoc = "@code{\\retrograde} can deal with crescendo and
  decrescendo as long as they are properly paired with
  @code{\\endcr}/@code{\\!} and @code{\\enddecr}.  Direction modifiers
  on slurs like @code{^(} need to be repeated as @code{^)} at the end.
  Ties and glissandi work mostly (in-chord ties are turned into
  ordinary per-chord/note ties, however)."
}

\layout { ragged-right = ##t }

motif =
\relative {
  \override TextSpanner.bound-details.left.text = "motif"
  <c' e>2~\startTextSpan  c16\< d^( e f~ f4:32^)\!\> |
  <<
    \context Voice = "voice" {
      <g~ b>4 g8\glissando f\stopTextSpan\enddecr }
    \\
    { c2 }
  >>
}

\new Voice = "voice" {
  \motif
  \override TextSpanner.bound-details.left.text = "retrograde motif"
  \retrograde  \motif \bar "|."
}
