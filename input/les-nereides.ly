\header{
    composer =   "ARTHUR GRAY";
    title =      "LES N\\'ER\\'EIDES";
    subtitle =   "THE NEREIDS";
    enteredby =  "JCN";
    piece =      "Allegretto scherzando";
    copyright =  "public domain";
    description = "Natiest piece of competition at http://www.orphee.com/comparison/study.html, see http://www.orphee.com/comparison/gray.pdf";
    comment =     "LilyPond (1.3.93) can't really do this yet, I guess";
}

global = \notes{
    \partial 2;
    \key a \major;
    \skip 2;
    \skip 1*2;

    %\skip 1;

    % fake grace
    \skip 2.; \partial 32*24;
    \skip 32*24;
    %end fake grace

    \bar "||";
}

treble = \context Voice=treble \notes\relative c''{
    r2
    %2
    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #1
    r4 <cis eis a cis> r2
    %3
    r4 <cis fis a cis> r8.
    \translator Staff=bass
    cis,16^2^"m.g."( <fis8 fis,\sustainup> <e! e,!>
    %4
    <)dis,4 a' dis>
    \translator Staff=treble
    \property basicSlurProperties \pop #'direction
    \property Voice.basicSlurProperties \push #'direction = #1
    %% 8va
    cis''''4 (

%{

    \property basicStemProperties \pop #'direction
    % ugh
    \property Voice.basicStemProperties \push #'direction = #0

    % grace is a mess
    % maybe we should fake this and put 5/4 in this bar?

    \grace {
        )cis8
	\property basicStemProperties \pop #'direction
	\property Grace.basicStemProperties \push #'direction = #0
	%urg, dim. during grace dumps core here
        %%[a16-5( fis dis] [cis'32 a-1 fis-4 dis\>] [cis a )\!fis-2]
        [a16-5( fis dis] [cis32 a-1 fis-4 dis] [cis a )fis-2]
    }

%}

    %% fake grace:
    \property basicNoteHeadProperties \pop #'font-size
    \property basicStemProperties \pop #'font-size
    \property basicBeamProperties \pop #'font-size
    \property basicTextScriptProperties \pop #'font-size
    \property basicSlurProperties \pop #'font-size
    \property basicLocalKeyProperties \pop #'font-size

    \property Voice.basicNoteHeadProperties \push #'font-size = #-1
    \property Voice.basicStemProperties \push #'font-size = #-1
    \property Voice.basicBeamProperties \push #'font-size = #-1
    \property Voice.basicTextScriptProperties \push #'font-size = #-1
    \property Voice.basicSlurProperties \push #'font-size = #-1
    \property Voice.basicLocalKeyProperties \push #'font-size = #-1

    )cis16
    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #0
    %%[a16^5( fis dis] [cis'32 a-1 fis-4 dis\>] [cis a )fis-2] s s s
    [a16^5( fis dis] [cis32 a^1 fis^4 dis\>] [cis a )fis-2] s % s s

    \property basicNoteHeadProperties \pop #'font-size
    \property basicStemProperties \pop #'font-size
    \property basicBeamProperties \pop #'font-size
    \property basicTextScriptProperties \pop #'font-size
    \property basicSlurProperties \pop #'font-size
    \property basicLocalKeyProperties \pop #'font-size

    %ugh ugh
    \property Voice.basicNoteHeadProperties \push #'font-size = #0
    \property Voice.basicStemProperties \push #'font-size = #0
    \property Voice.basicBeamProperties \push #'font-size = #0
    \property Voice.basicTextScriptProperties \push #'font-size = #0
    \property Voice.basicSlurProperties \push #'font-size = #0
    \property Voice.basicLocalKeyProperties \push #'font-size = #0
    %% end fake grace


    

    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #1
    \!cis'4()bis
    r8\mf-"a tempo"
    <a'8( a,> <gis gis,> <fis fis,> <gis gis,> <fis fis,> )e r|
    r\mf <a8( a,> <gis gis,> <fis fis,> <gis gis,> <fis fis,> )e r|
}

trebleTwo = \context Voice=trebleTwo \notes\relative c''{
    \skip 2;
    \skip 1*2;
    \skip 4;
    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #-1
    <cis4 a fis dis>

    %% fake grace
    \skip 32*16;
    %% end fake grace

    <e2-"rall." g, e d>
    r8 cis4. d4 [<cis8( e,> <b-3 d,-1> |
    <)a-2 cis,-1>] cis4. d4 [<cis8(\> e,> <b d,> |
    <\!)a cis,>]
}

bass = \context Voice=bass \notes\relative c{
    \property Voice.basicSlurProperties \push #'details =
        #'((height-limit . 2.0) (ratio . 0.333) (force-blowfit . 0.5) (beautiful . 5.0))
    \property basicStemProperties \pop #'direction
    \property Voice.basicSlurProperties \push #'direction = #-1
    % huh, auto-beamer?
    r8. e,16-2\f( [<a8 a,> <b b,>] <cis4\sustaindown cis,> |
    %2
    \translator Staff=treble
    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #-1
    \property Voice.slurEndAttachment = #'stem
    <)a''4 eis cis> 
    %\stemboth
    \property Voice.slurEndAttachment = ##f
    \translator Staff=bass
    \property basicStemProperties \pop #'direction
    % ugh
    \property Voice.basicStemProperties \push #'direction = #'0
    r8. cis,,16(\sustainup <fis8 fis,> <gis gis,>
    %3
    <a4\sustaindown a,>
    \translator Staff=treble
    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #-1
    <)a' fis cis>
    \translator Staff=bass
    \property basicStemProperties \pop #'direction
    % ugh
    \property Voice.basicStemProperties \push #'direction = #'0
    r2
    %4
    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #-1
    <b,,4\sustaindown b,>
    \clef treble;
    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #0
    <
        %urg: staff-change: ! on dis
        <cis'' a fis dis!>
%	{ s8. \sustainup\clef bass;}
    >
    %\grace { s16 s s s s32 s s s s \clef bass; s }
    %\clef bass;
    %\grace { <e,,,32\sustaindown( e,> } <)gis'2 e>

    %% fake grace
    s4 s8 s32 s \sustainup s \clef bass; s
    %% end fake grace

    \grace { <e,,,32( e,> } <)gis'2\sustaindown e>
    %5
    \property basicSlurProperties \pop #'direction
    \property Voice.basicSlurProperties \push #'direction = #1
    \property Staff.stopStartSustain = #""
    a,8\sustainup\sustaindown [e'-5(<)a-2 cis-3>]
    \property Staff.stopStartSustain = #"-P"
     r b,-5\sustainup\sustaindown <e4-3 gis-5 d'>
    \property basicSlurProperties \pop #'direction
    \property Voice.basicSlurProperties \push #'direction = #0
    [e,8-5(\sustainup|
    %6
    )a-2\sustaindown]
    \property basicSlurProperties \pop #'direction
    \property Voice.basicSlurProperties \push #'direction = #1
    [e'(<)a cis>] r b,\sustainup\sustaindown <e4 gis d'>
    \property basicSlurProperties \pop #'direction
    \property Voice.basicSlurProperties \push #'direction = #0
    [e,8(\sustainup|
    %7
    )a]
}

bassTwo = \context Voice=bassTwo \notes\relative c{
    \skip 2;
    \skip 1*2;
    \skip 2;

    %% fake grace
    \skip 32*16;

    \property basicStemProperties \pop #'direction
    \property Voice.basicStemProperties \push #'direction = #1
    \property basicSlurProperties \pop #'direction
    \property Voice.basicSlurProperties \push #'direction = #1
    cis'4()bis
}

\score{
    \context PianoStaff <
        \context Staff=treble <
	    \global
	    \treble
	    \trebleTwo
        >
        \context Staff=bass <
	    \clef bass;
	    \global
	    \bass
	    \bassTwo
        >
    >
    \paper {
	\translator {
	    \ScoreContext
	    basicTimeSignatureProperties \push #'style = #"C"
        }
	\translator {
	    \GraceContext
	    basicStemProperties \push #'flag-style = #""
        }
    }
}
