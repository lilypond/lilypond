%{
cpp -P -traditional -o l.ly les-nereides.ly
cpp -P -traditional -o l-fake.ly  -DFAKE_GRACE les-nereides.ly
%}



\header{
    composer =   "ARTHUR GRAY";
    title =      "LES N\\'ER\\'EIDES";
    subtitle =   "THE NEREIDS";
    enteredby =  "JCN";
    %piece =      "Allegretto scherzando";
    copyright =  "public domain";
    description = "Nastiest piece of competition at http://www.orphee.com/comparison/study.html, see http://www.orphee.com/comparison/gray.pdf";
    comment =     "LilyPond (1.3.93) can't really do this yet, I guess";
}


%% cpp: don't start on first column
 #(define (grace-beam-space-function multiplicity)
         (* (if (<= multiplicity 3) 0.816 0.844) 0.8))

%% cpp: don't start on first column
 #(define (make-text-checker text)
  (lambda (elt)
     ;; huh, string-match undefined?
     ;; (string-match text (ly-get-elt-property elt 'text))
     (equal? text (ly-get-elt-property elt 'text))
    ))


global =  \notes{
    \partial 2;
    \key a \major;
    \skip 2;
    \skip 1*2;

#ifndef FAKE_GRACE
    \skip 1;
#else % FAKE_GRACE
    \skip 2.; \partial 32*24;
    \skip 32*24;
#endif % FAKE_GRACE

    \bar "||";
}

treble =  \context Voice=treble \notes\relative c''{
    % Broken?
    \property Voice.NoteColumn \override #'horizontal-shift = #0
    \outputproperty #(make-type-checker 'text-item-interface) 
	    #'extra-offset = #'(-6 . 2)
    %% *Style* = Large??
    \property Voice.TextScript \override #'style = #"Large"
    r2^"Allegretto scherzando"
    \property Voice.TextScript \revert #'style
    %2
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #1
    r4 <cis\arpeggio eis a cis> r2
    %3
    r4 <cis\arpeggio fis a cis> r8.
    % Huh, urg?  Implicit \context Staff lifts us up to Staff context???
    \translator Staff=bass
    % Get back
    \context Voice 
    \outputproperty #(make-text-checker "m.d.")
	    #'extra-offset = #'(-3 . -4)
    % currently, this can't be (small) italic, because in the paperblock
    % we set italic_magnifictation to get large italics.
    cis,16^2^"m.d."( <fis8 fis,> <e! e,!>
    %4
    <)dis,4 a' dis>
    % Urg, this lifts us up to staff context
    \translator Staff=treble
    % Get back
    \context Voice 
    \property Voice.Slur \revert #'direction
    \property Voice.Slur \override #'direction = #1
    \property PianoStaff.connectArpeggios = ##t
    \property Voice.TextSpanner \revert #'type

    %% Ghostview is ok, but xdvi shows a solid line
    \property Voice.TextSpanner \override #'line-thickness = #2
    \property Voice.TextSpanner \override #'dash-period = #0.5

    \property Voice.TextSpanner \override #'type = #"dotted-line"
    \property Voice.TextSpanner \override #'edge-height = #'(0 . 1.5)
    \property Voice.TextSpanner \override #'edge-text = #'("8 " . "")
    % Huh, urg?  Implicit \context Staff lifts us up to Staff context???

	% no, see seq-mus-iter --hwn
    \property Staff."c0-position" = #-13


    % Get back
    %\context Voice 
    \outputproperty #(make-text-checker "m.g.")
	    #'extra-offset = #'(-3 . -2)
    % currently, this can't be (small) italic, because in the paperblock
    % we set italic_magnifictation to get large italics.
    cis''''4^"m.g."\arpeggio \spanrequest \start "text"  (

#ifndef FAKE_GRACE

    \property Voice.Stem \revert #'direction

    % grace is a mess
    % maybe we should fake this and put 5/4 in this bar?

    \grace {
        )cis8
	\property Grace.Stem \revert #'direction
	\property Grace.Stem \override #'direction = #0
	\property Grace.Beam \override #'space-function = #grace-beam-space-function
	%% urg, dim. during grace dumps core here
        %% [a16-5( fis dis \spanrequest \stop "text" ]
	%% [cis'32 a-1 fis-4 dis] [cis a )fis-2]
        [a16-5( fis dis \spanrequest \stop "text" ]
        \property Staff."c0-position" = #-6
	[cis32 a-1 fis-4 dis] [cis a )fis-2]
    }

#else % FAKE_GRACE 

    \property Score.PaperColumn  \override #'space-factor = #0.6
    \property Score.PaperColumn  \override #'to-musical-spacing-factor = #0.04
    \property Voice.NoteHead \override #'font-relative-size = #-1
    \property Voice.Stem \override #'font-relative-size = #-1
    \property Voice.Stem \override #'length = #6
    \property Voice.Stem \override #'beamed-lengths =
        #(map (lambda (x) (* 1.25 x)) '(0.0 2.5 2.0 1.5))
    \property Voice.Stem \override #'beamed-minimum-lengths =
        #(map (lambda (x) (* 1.25 x)) '(0.0 1.5 1.25 1.0))

    \property Voice.Beam \override #'font-relative-size = #-1
    \property Voice.TextScript \override #'font-relative-size = #-1
    \property Voice.Fingering \override #'font-relative-size = #-1
    \property Voice.Slur \override #'font-relative-size = #-1
    \property Staff.Accidentals \override #'font-relative-size = #-1
    \property Voice.Beam \override #'space-function = #grace-beam-space-function

    )cis16
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #0
    %% [a16^5( fis dis \spanrequest \stop "text" ]
    %% [cis'32 a-1 fis-4 dis] [cis a )fis-2] s s s
    [a16^5( fis dis \spanrequest \stop "text" ]
    \property Staff."c0-position" = #-6
    [cis32 a^1 fis^4 dis] [cis a )fis-2] s % s s

    \property Voice.NoteHead \revert #'font-relative-size
    \property Voice.Stem \revert #'font-relative-size
    \property Voice.Stem \revert #'length
    \property Voice.Stem \revert #'beamed-lengths
    \property Voice.Stem \revert #'beamed-minimum-lengths
    \property Voice.Beam \revert #'font-relative-size
    \property Voice.TextScript \revert #'font-relative-size
    \property Voice.Fingering \revert #'font-relative-size
    \property Voice.Slur \revert #'font-relative-size
    \property Staff.Accidentals \revert #'font-relative-size
    \property Voice.Beam \revert #'space-function
    \property Score.PaperColumn  \revert #'space-factor
    \property Score.PaperColumn \revert #'to-musical-spacing-factor
#endif % FAKE_GRACE
    

    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #1
    cis'4()bis
    r8
    <a'8( a,> <gis gis,> <fis fis,> <gis gis,> <fis fis,> )e^" "^1^4^5 r|
    r<a8( a,> <gis gis,> <fis fis,> <gis gis,> <fis fis,> )e r|
}

trebleTwo =  \context Voice=trebleTwo \notes\relative c''{
    \property Voice.NoteColumn \override #'horizontal-shift = #1
    s2
    s1*2
    s4
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #-1
    <cis'4\arpeggio a fis dis>

#ifdef FAKE_GRACE
    s32*16
#endif

    \property Voice.NoteColumn \override #'force-hshift = #-0.2
    <e,2 gis, e d!>
    % Hmm s/r?
    %r8 cis4. d4
    s8 cis4. d4
    \property Voice.NoteColumn \revert #'force-hshift
    [<cis8( e,> <b-3 d,-1> |
    \property Voice.NoteColumn \override #'force-hshift = #-0.2
    <)a-2 cis,-1>] cis4. d4 
    \property Voice.NoteColumn \revert #'force-hshift
    [<cis8( e,> <b d,> |
    <)a cis,>]
}

bass =  \context Voice=bass \notes\relative c{
    % Allow ugly slurs
    \property Voice.Slur \override #'beautiful = #5.0
    \property Voice.Slur \override #'attachment-offset = #'((0 . -3) . (0 . -6))
    \property Voice.Stem \revert #'direction
    \property Voice.Slur \override #'direction = #-1
    % huh, auto-beamer?
    r8. e,16-2( [<a8 a,> <b b,>] <cis4 cis,> |
    %2
    % Huh, urg?  Implicit \context Staff lifts us up to Staff context???
    \translator Staff=treble
    % Get back
    \context Voice 
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #-1
    \property Voice.slurEndAttachment = #'stem
    <)a''4\arpeggio eis cis> 
    %\stemBoth
    \property Voice.slurEndAttachment = ##f
    % Huh, urg?  Implicit \context Staff lifts us up to Staff context???
    \translator Staff=bass
    % Get back
    \context Voice 
    \property Voice.Stem \revert #'direction
    \property Voice.Slur \revert #'y-free
    \property Voice.Slur \override #'y-free = #0.1
    \property Voice.Slur \revert #'attachment-offset
    \property Voice.Slur \override #'attachment-offset = #'((0 . -3) . (0 . -8))
    r8. cis,,16( <fis8 fis,> <gis gis,>
    %3
    \property Voice.Stem \revert #'length
    \property Voice.Stem \override #'length = #5
    <a4 a,>
    % Huh, urg?  Implicit \context Staff lifts us up to Staff context???
    \translator Staff=treble
    % Get back
    \context Voice 
    \property Voice.Stem \revert #'length
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #-1
    <)a'\arpeggio fis cis>
    % Huh, urg?  Implicit \context Staff lifts us up to Staff context???
    \translator Staff=bass
    % Get back
    \context Voice 
    \property Voice.Stem \revert #'direction
    r2
    %4
    \property Voice.Slur \revert #'beautiful
    \property Voice.Slur \revert #'attachment-offset
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #-1
    <b,,4 b,>
    \clef treble;
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #0
    <
        %urg: staff-change: ! on dis
        <cis''\arpeggio a fis dis!>
%	{ s8. \clef bass;}
    >

#ifndef FAKE_GRACE
    %Hmm
    %\grace { s16 s s s s32 s s s s \clef bass; s }
    \clef bass;
    \grace { <e,,,32( e,> } <)gis'2 e>
#else
    s4 s8 s32 s  s \clef bass;
    \property Score.PaperColumn  \override #'space-factor = #0.6
    \property Score.PaperColumn  \override #'to-musical-spacing-factor = #0.1
    \property Voice.NoteHead \override #'font-relative-size = #-1
    \property Voice.Stem \override #'font-relative-size = #-1
    \property Voice.Stem \override #'length = #6
    \property Voice.Slur \override #'font-relative-size = #-1
    \property Voice.Slur \override #'attachment-offset = #'((-0.5 . 0) . (0.5 . 0))
    <e,,,32( e,>

    \property Voice.NoteHead \revert #'font-relative-size
    \property Voice.Stem \revert #'font-relative-size
    \property Voice.Stem \revert #'length
    \property Voice.Slur \revert #'font-relative-size
    \property Score.PaperColumn  \revert #'space-factor
    \property Score.PaperColumn \revert #'to-musical-spacing-factor
     <)gis'2 e>
    \property Voice.Slur \revert #'attachment-offset
#endif
    %5
    \property Voice.Slur \revert #'direction
    \property Voice.Slur \override #'direction = #1
    a,8 [e'-5(<)a-2 cis-3>]
    r b,-5 <e4-3 gis-5 d'>
    \property Voice.Slur \revert #'direction
    \property Voice.Slur \override #'direction = #0
    [e,8-5(|
    %6
    )a-2]
    \property Voice.Slur \revert #'direction
    \property Voice.Slur \override #'direction = #1
    [e'(<)a cis>] r b, <e4 gis d'>
    \property Voice.Slur \revert #'direction
    \property Voice.Slur \override #'direction = #0
    [e,8(|
    %7
    )a]
}

bassTwo =  \context Voice=bassTwo \notes\relative c{
    \skip 2;
    \skip 1*2;
    \skip 2;

#ifdef FAKE_GRACE
    \skip 32*16;
#endif

    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #1
    \property Voice.Slur \revert #'direction
    \property Voice.Slur \override #'direction = #1

    cis'4()bis
}

middleDynamics =  \context Dynamics=middle \notes{
    s8 s16 s\f s4
    s1*2
    %4
    s2

#ifdef FAKE_GRACE
    s32*12
    \outputproperty #(make-type-checker 'dynamic-interface) 
	    #'extra-offset = #'(0 . 1.5)
    s32\> s s \!s
#endif
    s32 
    \outputproperty #(make-type-checker 'text-item-interface) 
	    #'extra-offset = #'(0 . 1.5)
    s-"rall." s s
    s8 s4

    \outputproperty #(make-type-checker 'dynamic-interface) 
	    #'extra-offset = #'(0 . 4)
    %s1\mf-"a tempo"
    s2\mf-"a tempo" s4
    \outputproperty #(make-type-checker 'crescendo-interface) 
	    #'extra-offset = #'(0.5 . -1)
    s\> \!s8
    \outputproperty #(make-type-checker 'dynamic-interface) 
	    #'extra-offset = #'(-1 . 4)
    s8\mf s4 s4 s8\> s32 s s \!s
}

lowerDynamics =  \context Dynamics=lower \notes{
    s2
    %2
    s2\sustainDown s8. s16\sustainUp s4
    %3
    s2\sustainDown s8. s16\sustainUp s4
    %3

#ifndef FAKE_GRACE
    s4\sustainDown s16
    s32 s s\sustainUp s
    s32\sustainDown s s s
    s8
#else
    s2\sustainDown
    s32*12
    s32 s s\sustainUp s
    s32\sustainDown s s s
#endif

    \property Dynamics.stopSustain = #""
    s4 s16. s32\sustainUp

    %5
    s8\sustainDown s s
    \property Dynamics.stopSustain = #"*"
    \property Dynamics.stopStartSustain = #"-P"
    s s\sustainUp\sustainDown s s
    s\sustainUp

    %6
    \property Dynamics.stopStartSustain = #""
    s8\sustainDown s s
    \property Dynamics.stopStartSustain = #"-P"
    s s\sustainUp\sustainDown s s
    s\sustainUp
}

\score{
    \context PianoStaff <
        \context Staff=treble <
	    \global
	    \treble
	    \trebleTwo
        >
	\context Dynamics=middle <
	    \global
	    \middleDynamics
	>
        \context Staff=bass <
	    \clef bass;
	    \global
	    \bass
	    \bassTwo
        >
	\context Dynamics=lower <
	    \global
	    \lowerDynamics
	>
    >
    \paper {
	%%BURP
	magnification_italic = 1.;
	\translator {
	    \ScoreContext
	    TimeSignature \override #'style = #'C
	    SpacingSpanner \override #'maximum-duration-for-spacing = #(make-moment 1 4)

	    \remove Bar_number_engraver;
        }
	\translator {
	    \type "Engraver_group_engraver";
	    \name Dynamics;
	    \consists "Output_property_engraver";
	    Generic_property_list = #generic-voice-properties
	    \consists "Property_engraver";
	    DynamicsMinimumVerticalExtent = #(cons -3 -3)
	    VerticalAlignment \override #'threshold = #'(9 . 9) 

	    startSustain = #"Ped."
	    stopSustain = #"*"
	    stopStartSustain = #"*Ped."
	    startUnaChorda = #"una chorda"
	    stopUnaChorda = #"tre chorde"
	    
	    \consists "Piano_pedal_engraver";
	    \consists "Script_engraver";
	    \consists "Dynamic_engraver";
	    \consists "Text_engraver";
	    %GURGURGU, text is initialised using TextScript
	    TextScript \override #'style = #"italic"
	    %%% TextScript \override #'font-relative-size = #2

	    \consists "Skip_req_swallow_translator";

	    \consistsend "Axis_group_engraver";
    	}

	\translator {
	    \PianoStaffContext
	    \accepts Dynamics;
	    VerticalAlignment \override #'threshold = #'(7 . 7)
        }
	\translator {
	    \GraceContext
	    Stem \override #'flag-style = #""
        }
    }
}
