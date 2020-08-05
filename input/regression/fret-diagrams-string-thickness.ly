\version "2.17.6"

\header {
  texidoc = "String thickness can be changed, and diagrams can have variable
string thickness.  The thick zero-fret is adjusted accordingly for changed
@code{size}, @code{fret-diagram-details.string-thickness-factor} and
@code{fret-diagram-details.top-fret-thickness}.  There should be no visible gap
inside the red circle."
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
    c'1 ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

    %% C major for guitar,  verbose style
    \once
      \override TextScript.thickness = #1.5
    c'1 ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

    %% C major for guitar, verbose style
    \once
      \override TextScript.fret-diagram-details.string-thickness-factor = #0.3
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

    %% C major for guitar,  no barre,  verbose style
    c'1 ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 2)
                                     (place-fret 4 2 2)
                                     (open 3)
                                     (place-fret 2 1 1)
                                     (open 1))}

    %% C major for guitar,  no barre,  verbose style
    \once
      \override TextScript.fret-diagram-details.string-thickness-factor = #0.6
    \once
      \override TextScript.fret-diagram-details.top-fret-thickness = 16
    c' ^\markup \overlay {
            \translate #'(-3 . 1)
            \with-color #red
            \draw-circle #2 #0.1 ##f
            \override #'(size . 2)
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 2)
                                     (place-fret 4 2 2)
                                     (open 3)
                                     (place-fret 2 1 1)
                                     (open 1))}
 }
>>


