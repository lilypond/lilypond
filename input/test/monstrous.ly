

%% Chord gurus, is this ok now??

monstrous= \chords{
    \property Score.chordInversion = ##t 

    % /c is/was missing:
    bes:6/+c

    % Cdim7
    c:dim7
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
