\version "2.23.14"

\header {
  texidoc = "Tuplet brackets can be set to always be printed when
the direction of the bracket is forced to be on the note head side.
This setting doesn't have any effect on kneed tuplets."
}


{
  \tupletUp
  \tuplet 3/2 { c''8 d'' e'' }
  \tupletNeutral
  \relative c'' {
  \tuplet 3/2 { c16[ d e } f8]
  \tuplet 3/2 { c8 d e }
  }
}

{
  \override TupletBracket.visible-over-note-heads = ##t
  \tupletUp
  \tuplet 3/2 { c''8 d'' e'' }
  \tupletNeutral
  \relative c'' {
  \tuplet 3/2 { c16[ d e } f8]
  \tuplet 3/2 { c8 d e }
  }
}

{
  \override TupletBracket.bracket-visibility = #'if-no-beam
  \tupletUp
  \tuplet 3/2 { c''8 d'' e'' }
  \tupletNeutral
  \relative c'' {
  \tuplet 3/2 { c16[ d e } f8]
  \tuplet 3/2 { c8 d e }
  }
}

{
  \override TupletBracket.bracket-visibility = #'if-no-beam
  \override TupletBracket.visible-over-note-heads = ##t
  \tupletUp
  \tuplet 3/2 { c''8 d'' e'' }
  \tupletNeutral
  \relative c'' {
  \tuplet 3/2 { c16[ d e } f8]
  \tuplet 3/2 { c8 d e }
  }
}

{
  \override TupletBracket.visible-over-note-heads = ##t
  \tuplet 3/2 {
    c16[ 16 16
    c'''16 16 16]
  }
  \override TupletBracket.direction = #DOWN
  \tuplet 3/2 {
    c16[ 16 16
    c'''16 16 16]
  }
}

