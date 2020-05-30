
\header
{
  texidoc = "horizontal scripts are ordered, so they do not overlap.
The order may be set with script-priority.

The scripts should not be folded under the time signature.
"
  
}
\version "2.19.21"

\paper {
  ragged-right = ##t
}

\relative 
{
  \set stringNumberOrientations = #'(left) 
  \set fingeringOrientations = #'(left) 
  \set strokeFingerOrientations = #'(left) 
  <cis'-1\4\rightHandFinger #1 f>\arpeggio
}
