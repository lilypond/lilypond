\version "2.25.28"

\header {
  texidoc = "This placeholder test covers some internals of a future
@code{\\senzaMisura} command."
}

#(ly:set-option 'warning-as-error #t)

#(when (defined? 'senzaMisura) (error "update this test"))
senzaMisura =
#(define-music-function () ()
  (make-music 'TimeSignatureMusic 'time-signature #f))

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    \senzaMisuraTimeSignatureX
 }
}

testMusic = {
  \contextPropertyCheck Score.beamExceptions #'()
  \contextPropertyCheck Score.beatBase #+inf.0
  \contextPropertyCheck Score.beatStructure #'()
  \contextPropertyCheck Score.measureLength #+inf.0
  \contextPropertyCheck Score.timeSignatureFraction ##f
  \repeat unfold 8 { 8 }
}

\new Score \with { \senzaMisura } \testMusic
\new Score { \senzaMisura \testMusic }
