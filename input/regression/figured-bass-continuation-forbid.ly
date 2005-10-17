\header {

  texidoc = "By adorning a bass figure with @code{\\!}, an extender
  may be forbidden. "

}
\version "2.7.13"
\paper { raggedright = ##t }

\figures {
  \set useBassFigureExtenders = ##t
  <4 6 7> 
  <4\! 6 7->
 }
