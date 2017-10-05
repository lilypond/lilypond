\version "2.21.0"

\header {
  texidoc = "Harp pedals can be tweaked through the size, thickness and 
harp-pedal-details properties of TextScript."
}

\relative {
  \override Score.PaperColumn.keep-inside-line = ##f
  \override Voice.TextScript.harp-pedal-details.box-width = #1
  \once \override Voice.TextScript.size = #1.5
  \once \override Voice.TextScript.thickness = #7
  c''1^\markup \harp-pedal "o^ovo-|vovo-o^"
  c1^\markup \override #'(harp-pedal-details . (
                  (box-width . 0.6)
                  (box-height . 0.3)
                  (box-offset . 0.5)
                  (space-before-divider . 0.1)
                  (space-after-divider . 1.2))) {
           \harp-pedal "o^ovo-|vovo-o^"}
  \revert Voice.TextScript.harp-pedal-details.box-width
  \override Voice.TextScript.harp-pedal-details.circle-thickness = #3
  \override Voice.TextScript.harp-pedal-details.circle-x-padding = #0.6
  \override Voice.TextScript.harp-pedal-details.circle-y-padding = #-0.3
  c1^\markup \harp-pedal "o^ovo-|vovo-o^"
}
