\version "2.25.31"

\header {
  texidoc = "Toe-heel substitution commands behave similar to
@code{\\rtoe} and siblings if @code{Toe_heel_engraver} is enabled:
They can be styled, and direction indicators have no effect."
}


motif = #(define-music-function (style) (symbol?)
  #{
    \set toeHeelStyle = #style
    <>^\markup \typewriter #(symbol->string style)
    g'4_\rtoeheel g'\rheeltoe e'4^\ltoeheel e'\lheeltoe
  #})

music = {
  \override TextScript.staff-padding = #3
  \motif #'default
  \motif #'standard
  \motif #'reversed
  \motif #'below
  \motif #'circleheels }


\markup { Without \typewriter Toe_heel_engraver }
\markup \vspace #0.5
\new Voice \with {
  \remove Toe_heel_engraver
} \music

\markup { With \typewriter Toe_heel_engraver }
\markup \vspace #0.5
\new Voice \music
