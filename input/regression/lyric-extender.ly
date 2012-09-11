\version "2.16.0"

\header {

    texidoc= "A LyricExtender may span several notes.  A LyricExtender
does not extend past a rest, or past the next lyric syllable."

}

\layout { ragged-right = ##t }

\relative c''{
    d8( e f) r4. 
    f4
    d8[( e)] f
}
\addlyrics { ah __ ha a __ haaaaaaaaaaaa }




