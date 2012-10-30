\version "2.17.6"

\header {
  texidoc="
A capo indicator can be added with a fret-diagram-verbose
string, and its thickness can be changed.

"
}

\layout { ragged-right = ##t }

<<
  \chords {
    c1 |
    c1
  }

  \new Voice {
    \textLengthOn

     %% C major for guitar, with capo on third fret
        % verbose style
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (capo 3)
                                     (open 5)
                                     (place-fret 4 5 1)
                                     (place-fret 3 5 2)
                                     (place-fret 2 5 3)
                                     (open 1))}

     %% C major for guitar, with capo on third fret
       % thinner capo
        % verbose style
    \override TextScript.fret-diagram-details.capo-thickness = #0.2
    c' ^\markup {
            \fret-diagram-verbose #'((mute 6)
                                     (capo 3)
                                     (open 5)
                                     (place-fret 4 5 1)
                                     (place-fret 3 5 2)
                                     (place-fret 2 5 3)
                                     (open 1)) }

 }
>>


