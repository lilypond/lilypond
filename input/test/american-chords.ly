#(set! chord::names-alist-american
      (append 
      '(
	;; any changes here, see scm/chord-names.scm

	)
      chord::names-alist-american))

chord = \notes\transpose c''\chords{
\property ChordNames.chordNameStyle = "american"
c
c:m
c:m5-
c:5^3
c:4^3
c:5+
c:2^3
c:m5-.7-
c:7+
c:7.4^3
c:5+.7
c:m5-.7
c:5-.7+
c:m7+
c:m7
c:7
c:6
c:m6
c:9^7
c:6.9^7
c:9
c:7+.9
}

\score{
<
\context ChordNames \chord
\context Staff \chord
>
    \paper
    {
        \translator { \ChordNameContext chordNameWordSpace = #1 }
        \translator { \LyricsContext textScriptWordSpace = #0.3 }
    }
}

%    (((0 . 0) (2 . -1) (4 . 0)) . ("Bar" . ("script" . "Baz")))
