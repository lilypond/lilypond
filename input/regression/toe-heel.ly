\version "2.25.31"

\header {
  texidoc = "Commands for toe and heel pedal marks can be styled
using the @code{toeHeelStyle} context property if
@code{Toe_heel_engraver} is enabled.  If styled, direction
indicators have no effect."
}


motif = #(define-music-function (style) (symbol?)
  #{
    \set toeHeelStyle = #style
    <>^\markup \typewriter #(symbol->string style)
    g'4_\rtoe g'\rheel e'4^\ltoe e'\lheel
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
