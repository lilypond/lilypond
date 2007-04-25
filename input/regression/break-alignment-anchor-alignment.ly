\version "2.11.22"

\header {
  texidoc = "The default callback for break-align-anchor in clefs and time/key
signatures reads the break-align-anchor-aligment property to align
the anchor to the extent of the break-aligned grob."
}

{
  \override Score.RehearsalMark #'break-align-symbol = #'key-signature
  c1
  \key cis \major
  \once \override Staff.KeySignature #'break-align-anchor-alignment = #LEFT
  \mark \default
  cis1
  \key ces \major
  \once \override Staff.KeySignature #'break-align-anchor-alignment = #RIGHT
  \mark \default
  ces1
}