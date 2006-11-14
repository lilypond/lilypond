\header {

  texidoc = "By adorning a bass figure with @code{\\!}, an extender
  may be forbidden. "

}
\version "2.10.0"
\paper { ragged-right = ##t }

\figures {
  \set useBassFigureExtenders = ##t
  <4 6 7> 
  <4\! 6 7->
 }
