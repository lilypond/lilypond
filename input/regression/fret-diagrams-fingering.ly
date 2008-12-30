\version "2.12.0"

\header {
  texidoc="
Finger labels can be added, either in dots or below strings.
Dot color can be changed, and fingering label font size
can be adjusted.

"
}

\layout { ragged-right = ##t }

<<
  \chords {
    c1 |
    c1 |
    c1 |
    c1 |
    c1
  }

  \new Voice {
    \textLengthOn
     %% C major for guitar,  verbose style
    \override TextScript #'fret-diagram-details
      #'finger-code = #'below-string
    c'1 ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 1))}

     %% C major for guitar,  verbose style
    \override TextScript #'fret-diagram-details 
        #'string-label-font-mag = #0.4
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
      #'finger-code = #'in-dot
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 1))}

     %% C major for guitar, verbose style
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
        #'dot-label-font-mag = #0.75
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


