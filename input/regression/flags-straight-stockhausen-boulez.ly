\version "2.19.22"

\header {
texidoc = "Flags can be drawn straight in the style used by
Stockhausen and Boulez.
"
}


stemLength = #(define-music-function (length) (number?)
  "Set the length of the next stem explicitly."
  #{
    \once \override Stem.length-fraction = #length
  #}
)

{
    \autoBeamOff
    \time 3/8
    \override Flag.stencil = #modern-straight-flag
    \override Stem.length-fraction = #'1.5
    r8
    \acciaccatura {
         \stemDown
         \slurUp
         \stemLength #1
         gis''8
         \stemNeutral
       } \stemLength #1.43 d'8 r16 
       \acciaccatura { \stemLength #0.95 c''8 }
       \stemLength #1.25
       b'32 r
    \bar"|.|"
    \stemLength #1.25
    <g! cis'>16 \stemLength #1.3 <f'! g''!>8
}
