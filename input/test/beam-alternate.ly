
\header {

    texidoc = "The eighth notes may be seemingly attached to different
    beams, and the corresponding notes connected by ties (see also
    @file{tie-cross-voice.ly}). 
    Such a situation may occur, for example, in the cello suites."

}

\version "2.3.8"

wipeNote = {
    \once \override NoteHead #'transparent = ##t
    \once \override Stem #'transparent = ##t 
}
\paper { raggedright = ##t }


\relative c''<< {
    c8[~
       \wipeNote
       c8
       c8~
       \wipeNote
       c
       c]~
    \wipeNote
    c\noBeam
}\\
   { s8 c8 [ s c s c] }

   
>>
