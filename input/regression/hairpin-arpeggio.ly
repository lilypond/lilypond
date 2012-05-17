\version "2.15.37"

\header {
  texidoc = "Hairpins in @code{Dynamics} contexts do not collide with
arpeggios.
"
}

\new PianoStaff<<
 \set PianoStaff.connectArpeggios = ##t
 { c''\arpeggio c'' }
 \new Dynamics { s\< s\! }
 { c'\arpeggio c'' }
>>
