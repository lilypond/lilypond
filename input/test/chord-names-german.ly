
\version "1.7.16"
\header  {

    texidoc = "By setting @code{ChordNames.chordRootNamer}, the root
 of the chord may be named with a different function."

}

scm = \chords { c4 b bes } 
\score {

<    \context ChordNames \chords <
    \property ChordNames. chordRootNamer = #note-name->german-markup
    \scm >
    \context Voice \scm >
\paper { raggedright = ##t }
}
