\version "2.21.0"

\header {

    texidoc= "A vowel transition arrow may span several
notes.  The arrow may extend past a rest, but not past
the next lyric syllable."

}

\layout { ragged-right = ##t }

\relative c''{
    d8( e f) r4.
    f4
    d8[( e)] f
}
\addlyrics {
    ah \vowelTransition oh
    ah \vowelTransition oh }
