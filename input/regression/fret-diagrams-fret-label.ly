\version "2.21.0"

\header {
  texidoc="
The label for the lowest fret can be changed in location,
size, and number type.

"
}

\layout { ragged-right = ##t }

<<
  \chords {
    c1 |
    c1 |
    c1 |
    c1
  }

  \new Voice {
    \textLengthOn

     %% C major for guitar,  verbose style
    c'1 ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

     %% C major for guitar, verbose style
    \override TextScript.fret-diagram-details.label-dir = #LEFT
    \override TextScript.fret-diagram-details.number-type = #'roman-upper
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

     %% C major for guitar, verbose style
    \revert TextScript.fret-diagram-details.label-dir
    \override TextScript.fret-diagram-details.fret-label-font-mag = #0.4
    \override TextScript.fret-diagram-details.number-type = #'arabic
    \once \override TextScript.fret-diagram-details.fret-label-vertical-offset = #0.25
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

     %% C major for guitar, verbose style
    \revert TextScript.fret-diagram-details.label-dir
    \once \override TextScript.fret-diagram-details.number-type = #'custom
    \once \override TextScript.fret-diagram-details.fret-label-horizontal-offset = #0.5
    \once \override TextScript.fret-diagram-details.fret-label-custom-format = "~d°"
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

 }
>>
