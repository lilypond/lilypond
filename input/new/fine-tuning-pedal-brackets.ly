\version "2.11.61"

\header {
  lsrtags = "keyboards, tweaks-and-overrides"
  texidoc = "The appearance of pedal brackets may be altered in
different ways."
  doctitle = "Fine-tuning pedal brackets"
}

\paper { ragged-right = ##f }
\relative c'' {
  c2\sostenutoOn c
  c2\sostenutoOff c
  \once \override Staff.PianoPedalBracket #'shorten-pair = #'(-7 . -2)
  c2\sostenutoOn c
  c2\sostenutoOff c
  \once \override Staff.PianoPedalBracket #'edge-height = #'(0 . 3)
  c2\sostenutoOn c
  c2\sostenutoOff c
}
