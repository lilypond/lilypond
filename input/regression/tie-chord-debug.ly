\header {

  texidoc = "Switching on debug-tie-scoring annotates the tie scoring
decisions made."
  
}

\version "2.19.21"

\paper
{
  ragged-right = ##t
  debug-tie-scoring = ##t
}

\relative {
  <a' b e f> ~
  <a b e f>
}
