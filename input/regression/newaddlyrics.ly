\header {
    texidoc = "newlyrics -- multiple stanzas broken as yet."
}

%{

UGHr, fool lilypond-book:

\score

%}

%% Testing newlyrics 
%%\new PianoStaff <<
<<
    \new Staff \relative {
	d'2 d c4 bes a2 \break
	c2 c d4 f g2
    }
    \newlyrics <<
	{
	    My first Li -- ly song,
	    Not much can go wrong!
	}
	{
	    My se -- cond ly verse
	    Not much can go wrong!
	}
    >>
    \new Staff \relative {
	\clef bass
	d2 d c4 bes a2 \break
	c2 c d4 f g2
    }
    \newlyrics <<
	{
	    MY FIRST LI -- LY SONG,
	    NOT MUCH CAN GO WRONG!
	}
	{
	    MY SE -- COND LY VERSE
	    NOT MUCH CAN GO WRONG!
	}
    >>
>>

\version "2.3.0"
