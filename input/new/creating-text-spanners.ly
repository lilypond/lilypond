\version "2.11.65"

\header {
  lsrtags = "expressive-marks, text, tweaks-and-overrides"
  texidoc = "The @code{\\startTextSpan} and @code{\\stopTextSpan}
commands allow the creation of text spanners as easily as pedal
indications or octavations.  Override some properties of the
@code{TextSpanner} object to modify its output."

  doctitle = "Creating text spanners"
}

\paper { ragged-right = ##f }

\relative c'' {
  \override TextSpanner #'(bound-details left text) = #"bla"
  \override TextSpanner #'(bound-details right text) = #"blu"
  a4 \startTextSpan
  b4 c
  a4 \stopTextSpan
  
  \override TextSpanner #'style = #'line
  \once \override TextSpanner
    #'(bound-details left stencil-align-dir-y) = #CENTER
  a4 \startTextSpan
  b4 c
  a4 \stopTextSpan
  
  \override TextSpanner #'style = #'dashed-line
  \override TextSpanner #'(bound-details left text) =
    \markup { \draw-line #'(0 . 1) }
  \override TextSpanner #'(bound-details right text) =
    \markup { \draw-line #'(0 . -2) }
  \once \override TextSpanner #'(bound-details right padding) = #-2

  a4 \startTextSpan
  b4 c
  a4 \stopTextSpan
  
  \set Staff.middleCPosition = #-13
  \override TextSpanner #'dash-period = #10
  \override TextSpanner #'dash-fraction = #0.5
  \override TextSpanner #'thickness = #10
  a4 \startTextSpan
  b4 c
  a4 \stopTextSpan
}
