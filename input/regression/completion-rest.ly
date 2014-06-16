\version "2.19.0"

\header{
texidoc="

If the @code{Rest_engraver} is replaced by the @code{Completion_rest_engraver},
long rests, longer than @code{measureLength}, are split into
un-scaled rests, even if the original duration used a scale-factor.
@code{completionFactor} controls this behavior."
}

\layout { ragged-right= ##t }


\new Voice \with {
    \remove "Rest_engraver"
    \consists "Completion_rest_engraver"
} \relative c'{

  r\breve |
  r1*2 |
  r2*4 |
  r8*20 r2 \break
  \bar "||" \time 2/4
  r\breve.*2/3
  \set completionFactor = #1/2
  r\breve.*2/3^"explicity request r1*1/2 rests"
}
