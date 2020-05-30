\version "2.21.0"

\header {
  texidoc = "Skylines of boxes with and without rounded corners reflect
             the actual box outline even if rotated.
             Skylines of ellipses are stable when rotated."
}

#(ly:set-option 'debug-skylines #t)

\layout { indent = 0 }

{
  \override TextScript.padding = 0
  \override TextScript.outside-staff-padding = 0
  \override TextScript.outside-staff-horizontal-padding = 0
  \override TextScript.self-alignment-X = #CENTER
  \override TextScript.layer = 0
  f'''4
  c'' ^\markup \override #'(thickness . 4) \box \teeny 1
  _\markup \with-color #grey \filled-box #'(-1 . 4) #'(1 . 4) #1
  b'  ^\markup \rounded-box "2!" ^\markup \rounded-box "3"
  d'' ^\markup \circle "*" ^\markup \rotate #90 \ellipse "Elli"
  _\markup \with-color #grey \rotate #10 \filled-box #'(-2 . 5) #'(1 . 4) #10
  _ \markup \rotate #36 \override #'(corner-radius . 20) \scale #'(1 . 0.4)
  \rotate #-68 \scale #'(1.6 . 1) \rounded-box "dop"
  f'' _\markup \override #'(corner-radius . 0.5) \rounded-box "."
  a'  ^\markup \rotate #30 \rounded-box "rotated a"
  _\markup \scale #'(1 . 1.5) \rotate #45 \override #'(thickness . 5)
  \rounded-box \rotate #-45 \scale #'(2 . 1) \bold "!"
  d'' _\markup \override #'(corner-radius . 20) \rounded-box "4"
  a'' ^\markup \rotate #-165 \box "rotated b"
  _\markup \rotate #180 \override #'(corner-radius . 20) \rounded-box \teeny
  \override #'(baseline-skip . 0) \center-column { "Upside" "Down" }
}
