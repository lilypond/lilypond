\version "1.5.68"

\header {
    texidoc ="FIXME"
    }
%% Chord gurus, is this ok now??

monstrous= \chords{

%% fixme:
%%    \property Score.chordInversion = ##t 

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
	linelength=-1.0\mm
    }
}
