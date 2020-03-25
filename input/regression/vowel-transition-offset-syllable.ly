\version "2.21.0"

\header {

    texidoc= "When a syllable is horizontally offset,
the arrow should adjust accordingly."

}

\layout { ragged-right = ##t }

\relative c''{
    c4 d e( f ~ | \break
    f2) e | \break
    c1 |
}
\addlyrics {
    \once \override LyricText.X-offset = #-5
    a \vowelTransition
    \once \override LyricText.X-offset = #2
    b \vowelTransition c \vowelTransition
    \once \override LyricText.self-alignment-X = #LEFT
    ddddddddd \vowelTransition e
}
