\version "2.19.8"

\header {
  texidoc="
Dots indicating fingerings can be changed in location,
size, and coloring.
It is possible to parenthesize a single dot.  The color
of the paranthesis may be taken from dot or default.
A possible collision between parathesis and fret-label-
indication can be resolved by an override for
@code{fret-label-horizontal-offset} in @code{fret-diagram-details}.

"
}

\layout { ragged-right = ##t }

<<
  \chords {
    c1 |
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

     %% C major for guitar, verbose style
    \override TextScript.fret-diagram-details.dot-radius = #0.3
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

     %% C major for guitar, verbose style
    \revert TextScript.fret-diagram-details.dot-radius
    \once \override TextScript.fret-diagram-details.dot-color = #'white
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

     %% C major for guitar,  verbose style
    \once \override TextScript.fret-diagram-details.dot-position = #0.5
    c'1 ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1)
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3))}

     %% C major for guitar,  verbose style
    \once \override TextScript.size = #1.4
    c'1 ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (place-fret 5 3 1 blue)
                                     (place-fret 4 5 2 red parenthesized)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4 red)
                                     (place-fret 1 3 1 blue parenthesized)
                                     (barre 5 1 3))}

     %% C major for guitar,  verbose style
     %% larger size and dot-radius
     %% different dot-colors
     %% parenthesizing single dots, with different paren-color
     %% horizontal moving fret-label-indication
    \once \override TextScript.size = #1.5
    \once \override TextScript.fret-diagram-details.dot-radius = #0.4
    \once \override TextScript.fret-diagram-details.fret-label-horizontal-offset = #0.2
    c'1 ^\markup {
            \fret-diagram-verbose
              #'((mute 6)
                 (place-fret 5 3 1 blue)
                 (place-fret 4 5 2 red parenthesized default-paren-color)
                 (place-fret 3 5 3)
                 (place-fret 2 5 4 red)
                 (place-fret 1 3 1 blue parenthesized)
                 (barre 5 1 3))}

 }
>>
