
\version "1.7.18"
\header  {

    texidoc = "By setting @code{ChordNames.chordRootNamer}, the root
of the chord may be named with a different function."

}

scm = \chords { c4/c cis/cis cisis/cisis ces/ces ceses/ceses b/b bis/bis bes/bes beses/beses } 
\score {
<    \context ChordNames \chords <
    % #t gives true german chord-names
    % #f gives semi-german chord-names -
    % - with Bb and below keeping the english names
    \property ChordNames. chordRootNamer = #(chord-name->german-markup #f)
    \property ChordNames. chordNoteNamer = #note-name->german-markup
    \scm >
    \context Voice \scm >
}
