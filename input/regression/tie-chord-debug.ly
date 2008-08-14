\header {

  texidoc = "Switching on debug-tie-scoring annotates the tie scoring
decisions made."
  
}

\version "2.11.51"

\paper
{
  ragged-right = ##t
  debug-tie-scoring = ##t
}

\relative g' {
  <a b e f> ~
  <a b e f>
}
