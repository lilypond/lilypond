\version "2.12.0"

\header {
  texidoc="
Dots indicating fingerings can be changed in location,
size, and coloring.

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
                                     (barre 5 1 1))}

     %% C major for guitar, verbose style
    \override TextScript #'fret-diagram-details 
        #'dot-radius = #0.3
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 1))}

     %% C major for guitar, verbose style
    \revert TextScript #'fret-diagram-details #'dot-radius
    \once \override TextScript #'fret-diagram-details 
        #'dot-color = #'white
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 1))}

     %% C major for guitar,  verbose style
    \once \override TextScript #'fret-diagram-details 
        #'dot-position = #0.5
    c'1 ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 1))}

 }
>>


