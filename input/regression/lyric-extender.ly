\version "2.3.17"

\header {

    texidoc= "A LyricExtender may span several notes.  A LyricExtender
does not extend past a rest, or past the next lyric syllable."

}

\paper { raggedright = ##t }
\relative c''{
    d8( e f) r4. 
    f4
    d8[( e)] f
}
\addlyrics { ah __ ha a __ haaaaaaaaaaaa }




