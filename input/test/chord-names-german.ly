\version "1.7.20"
\header  {
    texidoc = "@cindex Chord Names German
By setting @code{ChordNames.chordRootNamer}, the root
of the chord may be named with a different function.
" }

scm = \chords { c1/c cis/cis cisis/cisis ces/ces ceses/ceses b/b bis/bis bes/bes beses/beses \bar "||" } 
\score {
\notes <
    % \germanChords gives true german chord-names
    % \semiGermanChords gives semi-german chord-names -
    % - with Bb and below keeping the english names
    \context ChordNames { \scm \germanChords \scm \semiGermanChords \scm }
    \context Voice { \scm s1*0^"germanChords" \scm s1*0^"semiGermanChords" \scm } >
}
