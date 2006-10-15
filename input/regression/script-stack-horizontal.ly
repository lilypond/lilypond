\header
{
  texidoc = "horizontal scripts are ordered, so they do not overlap.
The order may be set with script-priority."
  
}
\version "2.9.24"

\paper {
  ragged-right = ##t
}

\relative 
{
  \set stringFingerOrientations = #'(left) 
  \set fingeringOrientations = #'(left) 
  \set stringNumberOrientations = #'(left) 
  <cis-1\4
   -\rightHandFinger #1
   es-2\5
   -\rightHandFinger #2
   gis-3\6
   -\rightHandFinger #3
   >\arpeggio
}
