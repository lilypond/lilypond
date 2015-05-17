\header {

  texidoc = "Rests avoid notes.  Each rest is moved in the direction
of the stems in its voice.  Rests may overlap other rests in voices
with the same stem direction, in which case a warning is given, but
is suppressed if the rest has a pitch."

}

\version "2.19.21"

\new Staff <<
  \relative { g''8 g g r r2 } \\
  \relative { a4\rest c r2 } \\
  \relative { c''4 c f2\rest } \\
  \relative { r2 g' }
>>


