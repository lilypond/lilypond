
monstrous= \chords{
    \property Score.chordInversion = "1"

    % /c is missing:
    bes-6/c

    % Cdim7 ??
    cis-3-.5-.6 
}

\score{
    <
	\context ChordNames \monstrous
	\context Staff \monstrous
    >
    \paper{
	linelength=-1.0\mm;
    }
}
