#(ly:set-option 'old-relative)
\version "2.1.7"

%% I don't know what this example is supposed to do.  delete it?
%% It shows how to use the autochange feature together with an extra
%% voice.  When I needed to typeset this example, I found it not trivial
%% and dumped a piece of it here.  Move to refman?

\header{
    texidoc="When using automatic staff changes for the one voice, the
other voice must be given a name explicitely."
}

global = \notes{
    \key a \minor
    \time 6/4
}

melody = \notes\relative c''{
    r2 r r 
    r2 r r
    r4 a'8-- \< a-- a-- a-- c-- b\!-- a-\> gis f e\!
    es8 \grace b c r4 r2 r
}

basloopje = \notes\relative c{
    d,8( a' d f a d f d a f d a)
}

accompany = \repeat unfold 4 \notes \relative c \basloopje

\score{
    \notes \context PianoStaff<<
	\context Staff=up <<
	    \global
	    \context Voice=foo{
		\voiceOne
		\melody 
	    }
	>>
	\context Staff=down<<
	    \global
	    \clef bass
	    \autochange Staff \context Voice \accompany
	>>
    >>

    \paper {
	\translator{ 
	    \StaffContext
	    autoBeamSettings \override #'(end * * * *)
	    = #(ly:make-moment 1 2)
	}
    }
    \midi {
	\tempo 4 = 54
    }
}


