\version "1.6.5"

\header {
    composer =   "ARTHUR GRAY"
    title =      "LES N\\'ER\\'EIDES"
    subtitle =   "THE NEREIDS"
    enteredby =  "JCN"
    %piece =      "Allegretto scherzando"
    copyright =  "public domain"
    description = "Nastiest piece of competition at http://www.orphee.com/comparison/study.html, see http://www.orphee.com/comparison/gray.pdf"
}

#(set-point-and-click! 'line-column)
#(set! point-and-click line-column-location)

#(define (make-text-checker text)
  (lambda (elt) (equal? text (ly-get-grob-property elt 'text))))

global =  \notes{
    \partial 2
    \key a \major
    \skip 2
    \skip 1*2
    \skip 1
    \bar "||"
}

treble =  \context Voice=treble \notes\relative c''{
    % Broken?
    \property Voice.NoteColumn \override #'horizontal-shift = #0
    \outputproperty #(make-type-checker 'text-interface) 
	    #'extra-offset = #'(-6 . 2)
    r2^#'((Large upright) "Allegretto scherzando")
    %2
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #1
    r4 <cis\arpeggio eis a cis> r2
    %3
    r4 <cis\arpeggio fis a cis> r8.

    % Urg, this lifts us up to staff context
    \translator Staff=bass

    % Get back:
    \context Voice
    
    % If we don't get back to Voice, this text-checker doesn't work
    \outputproperty #(make-text-checker "m.d.")
	    #'extra-offset = #'(-3 . -4)
	    
    % currently, this can't be (small) italic, because in the paperblock
    % we set italic_magnifictation to get large italics.
    cis,16^2^"m.d."( <fis8 fis,> <e! e,!>
    %4
    <)dis,4 a' dis>
    
    % Urg, this lifts us up to staff context
    \translator Staff=treble
    % Get back:
    \context Voice
    
    \property Voice.Slur \revert #'direction
    \property Voice.Slur \override #'direction = #1
    \property PianoStaff.connectArpeggios = ##t
    \property Voice.TextSpanner \revert #'type

    %% Ghostview is ok, but xdvi shows a solid line
    \property Voice.TextSpanner \override #'thickness = #2
    \property Voice.TextSpanner \override #'dash-period = #0.5

    \property Voice.TextSpanner \override #'type = #'dotted-line
    \property Voice.TextSpanner \override #'edge-height = #'(0 . 1.5)
    %% \property Voice.TextSpanner \override #'edge-text = #'("8 " . "")
    \property Voice.TextSpanner \override #'edge-text = #'("8 " . "  ")

    % Urg, this lifts us up to staff context
    \property Staff.centralCPosition = #-13

    % Get back:
    \context Voice

    % If we don't get back to Voice, this text-checker doesn't work
    \outputproperty #(make-text-checker "m.g.")
	    #'extra-offset = #'(-3 . -3)
	    
    % currently, this can't be (small) italic, because in the paperblock
    % we set italic_magnifictation to get large italics.
    cis''''4^"m.g."\arpeggio \spanrequest \start "text" (

    \property Voice.Stem \revert #'direction

    \grace {
        )cis8
	\property Voice.Stem \revert #'direction
	\property Voice.Stem \override #'direction = #0
        [a16-5( fis dis \spanrequest \stop "text" ]
	\property Staff.centralCPosition = #-6
	
	[cis32 a-1 fis-4 dis] [cis a )fis-2]
    }

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
    \property Voice.Slur \override #'attachment-offset = #'((0 . 3) . (0 . -4))
    \property Voice.Stem \revert #'direction
    \property Voice.Slur \override #'direction = #-1
    % huh, auto-beamer?
    r8. e,16_2( [<a8 a,> <b b,>] |
    %2
    <cis4 cis,>
    % Huh, urg?  Implicit \context Staff lifts us up to Staff context???
    \translator Staff=treble
    % Get back
    \context Voice
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #-1
    \property Voice.Slur \override #'attachment = #'(stem . stem)
    <)a''4\arpeggio eis cis> 
    %\stemBoth
    \property Voice.Slur \revert #'attachment
    % Huh, urg?  Implicit \context Staff lifts us up to Staff context???
    \translator Staff=bass
    % Get back
    \context Voice 
    \property Voice.Stem \revert #'direction
    \property Voice.Slur \revert #'y-free
    \property Voice.Slur \override #'y-free = #0.1
    \property Voice.Slur \revert #'attachment-offset
    \property Voice.Slur \override #'attachment-offset = #'((0 . 3) . (0 . 8))
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
    \clef treble
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #0
    <
        %urg: staff-change: ! on dis
        <cis''\arpeggio a fis dis!>
%	{ s8. \clef bass}
    >

    %Hmm
    %\grace { s16 s s s s32 s s s s \clef bass s }
    \clef bass
    \grace { <e,,,32( e,> } <)gis'2 e>
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
    \skip 2
    \skip 1*2
    \skip 2

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

    s32 
    \outputproperty #(make-type-checker 'text-interface) 
	    #'extra-offset = #'(0 . 1.5)
    s-"rall." s s
    s8 s4

    \outputproperty #(make-type-checker 'dynamic-interface) 
	    #'extra-offset = #'(0 . 4)
    %s1\mf-"a tempo"
%%    s2\mf-"a tempo" s4
    s2\mf-"a tempo" s8
%%    s\> \!s8
    s16 s32 s64 \> s s8  s \!s8
    \outputproperty #(make-type-checker 'dynamic-interface) 
	    #'extra-offset = #'(-1 . 4)
%%    s8\mf s4 s4 s8\> s32 s s \!s
    s8\mf s4 s8 s16 s32 s64 \> s s16 s8 s32 s s s\! s128
}

lowerDynamics =  \context Dynamics=lower \notes{
    s2
    %2
    s2\sustainDown s8. s16\sustainUp s4
    %3
    s2\sustainDown s8. s16\sustainUp s4
    %3

    s4\sustainDown s16
    s32 s s\sustainUp s
    s32\sustainDown s s s
    s8

    \property Dynamics.pedalSustainStrings = #'("Ped." "*Ped." "")
    s4 s16. s32\sustainUp

    %5
    s8\sustainDown s s
    \property Dynamics.pedalSustainStrings = #'("Ped." "-P" "*")
    s s\sustainUp\sustainDown s s
    s\sustainUp

    %6
    \property Dynamics.pedalSustainStrings = #'("Ped." "*Ped." "")
    s8\sustainDown s s
    \property Dynamics.pedalSustainStrings = #'("Ped." "-P" "*")
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
	    \clef bass
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
        % Hmm
	% magnification_italic = 1.
	\translator {
	    \ScoreContext
	    TimeSignature \override #'style = #'C
	    %% SpacingSpanner \override #'maximum-duration-for-spacing = #(make-moment 1 4)

	    \remove Bar_number_engraver
        }
	\translator {
	    \type "Engraver_group_engraver"
	    \name Dynamics
	    \consists "Output_property_engraver"
	    %%Generic_property_list = #generic-voice-properties
	    %%\consists "Property_engraver"
	    minimumVerticalExtent = #'(-1 . 1)

	    pedalSustainStrings = #'("Ped." "*Ped." "*")
	    pedalUnaCordaStrings = #'("una corda" "" "tre corde")
	    
	    \consists "Piano_pedal_engraver"
	    \consists "Script_engraver"
	    \consists "Dynamic_engraver"
	    \consists "Text_engraver"

	    TextScript \override #'font-relative-size = #1
	    TextScript \override #'font-shape = #'italic

	    \consists "Skip_req_swallow_translator"

	    \consistsend "Axis_group_engraver"
    	}

	\translator {
	    \PianoStaffContext
	    \accepts Dynamics
	    VerticalAlignment \override #'forced-distance = #7
        }
	\translator {
	    \GraceContext
	    Stem \override #'stroke-style = #""
        }
    }
}

%%% Local variables:
%%% LilyPond-indent-level:4
%%% End:
