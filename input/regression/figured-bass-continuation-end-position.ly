\version "2.16.0"

\header {
  texidoc = "Figured bass extender for figures of different width (e.g. with
alteration or two-digit figures) should still stop at the same position."

}

\figures {
  \set useBassFigureExtenders = ##t
  \set figuredBassAlterationDirection = #RIGHT
  <6 5+ 3>4.
  <6 5+ 2>4.
  r4
  <12 5>4.
  <12 5>4.
  r4
}
