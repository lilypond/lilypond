\version "2.16.0"

\header {
  texidoc = "By adorning a bass figure with @code{\\!}, an extender may be
  forbidden."
}

\figures {
  \set useBassFigureExtenders = ##t
  <4 6 7>
  <4\! 6 7->
}
