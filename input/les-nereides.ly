#(ly:set-option 'old-relative)
\version "1.9.4"

\header {
    composer =   "ARTHUR GRAY"
    title =      "LES N\\'ER\\'EIDES"
    subtitle =   "THE NEREIDS"
    enteredby =  "JCN"
    piece =      "Allegretto scherzando"
    copyright =  "public domain"
}

%{

Nastiest piece of competition at
http://www.orphee.com/comparison/study.html, see
http://www.orphee.com/comparison/gray.pdf


lines that contain tweaks (17 currently, not counting reverts) are
marked with %tweak

possibly more impressive to render without tweaks?

  grep -v tweak input/les-nereides.ly >> lnnt.ly
  ly2dvi lnnt.ly

%}

#(ly:set-point-and-click 'line-column)
#(define (make-text-checker text)
  (lambda (elt) (equal? text (ly:get-grob-property elt 'text))))


treble = \new Voice \notes\relative c''{
    \key a \major
    r2
    | %2
    \stemUp
    r4 <cis eis a cis>\arpeggio r2
    | %3
    r4 <cis fis a cis>\arpeggio r8.

    \translator Staff=bass

    \once\property Voice.TextScript \set #'extra-offset = #'(-3 . -4) %tweak
	    
    cis,16^2(^\markup {\small \italic "m.d." }
    <fis fis,>8 <e! e,!>
    | %4
    <dis, a' dis>4)
    
    \translator Staff=treble
    
    \slurUp
    \property PianoStaff.connectArpeggios = ##t

    #(set-octavation 1)

    \once\property Voice.TextScript \set #'extra-offset = #'(-3 . -2) %tweak
	    
    \tieUp
    cis''''4^\markup { \small \italic "m.g." }\arpeggio~
    \grace {
	\property Voice.Stem \override #'stroke-style = #"grace"
  
         cis8
	 
         %\stemBoth Hmm
	
	 \property Voice.Stem \set #'direction = #0
	 
          a16[-5( fis dis]
	 #(set-octavation 0)
	
 	 cis32[ a-1 fis-4 dis]   cis[ a  fis)-2]
	 % the small grace in lower staff comes after us
	 s32
    
	\property Voice.Stem \revert #'stroke-style
    }


    \stemUp
    cis'4( bis)

    | %5
    r8 <a' a,>8(\mf <gis gis,> <fis fis,>
    
    % \fingerUp
    \property Voice.Fingering \set #'direction = #1
    
    % Manual fix for collision with slur
    \property Voice.Fingering \set #'extra-offset = #'(0 . 1) %tweak
    <gis gis,> <fis fis,> e)-1-4-5 r

    | %6
    r <a a,>8(\mf <gis gis,> <fis fis,>
    <gis gis,> <fis fis,>  e) r
    
    | %7
    \bar "||"
}

trebleTwo =  \new Voice \notes\relative c''{
    \stemDown
    \slurDown
    % \fingerDown
    \property Voice.Fingering \set #'direction = #-1
    \property Voice.Fingering \set #'extra-offset = #'(0 . 1.2)
    s2
    | %1
    s1*2
    | %4
    s4
    <cis' a fis dis>4\arpeggio
    <e, gis, e d!>2
    | %5
    s8 cis4. d4
    %%<cis e,>8[( <b-3 d,-1>
    <cis e,>8[( <b d,>-3-1
    | %6
    %%<a-2 cis,-1>)] cis4. d4 
    <a cis,>)]-2-1 cis4. d4 
    <cis e,>8[( <b d,>
    | %7
    <a cis,>)]
}

bass =  \new Voice \notes\relative c{
    \partial 2
    \key a \major
    
    % Allow ugly (highly blown-up) slurs
    \property Voice.Slur \override #'beautiful = #5.0 %tweak
    \property Voice.Slur \override #'attachment-offset = #'((0 . 3) . (0 . -4))  %tweak
    \slurDown
    
    \dynamicUp

    r8. e,16(\f_2 <a a,>8[ <b b,>]
    | %2
    <cis cis,>4
    \translator Staff=treble
    \stemDown
    \property Voice.Slur \override #'attachment = #'(stem . stem) %tweak
    <a'' eis cis>4)\arpeggio
    
    \property Voice.Slur \revert #'attachment %tweak
    \translator Staff=bass
    \stemBoth
    
    \property Voice.Slur \revert #'y-free %tweak
    \property Voice.Slur \override #'y-free = #0.1 %tweak
    \property Voice.Slur \revert #'attachment-offset %tweak
    \property Voice.Slur \override #'attachment-offset = #'((0 . 3) . (0 . 8)) %tweak
    r8. cis,,16( <fis fis,>8 <gis gis,>
    
    | %3
    \property Voice.Stem \set #'length = #5 %tweak
    <a a,>4
    \translator Staff=treble
			    
    \property Voice.Stem \revert #'length %tweak
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #-1
    <a' fis cis>)\arpeggio
    \translator Staff=bass
    \property Voice.Stem \revert #'direction
    r2
    
    | %4
    \property Voice.Slur \revert #'beautiful %tweak
    \property Voice.Slur \revert #'attachment-offset %tweak
    \stemDown
    <b,, b,>4
    \clef treble
    \stemBoth
    <<
        %urg: staff-change: ! on dis
        <cis'' a fis dis!>\arpeggio
    >>
    
    \grace {
	\property Voice.Stem \override #'stroke-style = #"grace"
  
        s8
        s16 s s
 	s32 s s
	s s s
	\clef bass
	<e,,, e,>32(
    
	\property Voice.Stem \revert #'stroke-style
    }
    <gis' e>2)
    
    | %5
    \slurUp
    
    % \fingerDown
    \property Voice.Fingering \set #'direction = #-1
    
    %%a,8 e'[-5(<a-2 cis-3>])
    a,8 e'[-5(<a cis>])-2-3
    %%r b,-5 <e-3 gis-5 d'>4
    r b,-5 <e gis d'>4-3-5
    \slurBoth
    \once \property Voice.Fingering \set #'extra-offset = #'(0 . -1) %tweak
    e,8[-5(
    
    | %6
    \once \property Voice.Fingering \set #'extra-offset = #'(0 . -1) %tweak
    a)-2]
    \slurUp
    e'[(<a cis>)] r b, <e gis d'>4
    \slurBoth
    e,8[(
    
    | %7
    a)]
}

bassTwo =  \new Voice \notes\relative c{
    \skip 2
    \skip 1*2
    \skip 2

    \stemUp
    \slurUp

    cis'4( bis)
}

middleDynamics = \notes{
    \property Dynamics.TextScript \set #'padding = #-1 %tweak
    s2
    s1*2
    | %4
    s2
    \grace {
  \property Voice.Stem \override #'stroke-style = #"grace"
  
    	   s8
    	   s16 s s
    	   s32 s
           \once\property Dynamics.Hairpin \set #'extra-offset = #'(0 . 2) %tweak
	   s\> s
     	   s32 s s s\!
    
  \property Voice.Stem \revert #'stroke-style }

    s32 s-"rall." s s s8 s4
    | %5
    s2-"a tempo" s8
    \once\property Dynamics.Hairpin \set #'extra-offset = #'(1 . 0) %tweak
    s \> s s
    | %6 
    s8\!
    s2 s8 s\> s
    | %7
    s8\!
}

lowerDynamics = \notes{
    s2
    | %2
    s2\sustainDown s8. s16\sustainUp s4
    | %3
    s2\sustainDown s8. s16\sustainUp s4
    | %4
    s4\sustainDown
    \property Dynamics.pedalSustainStrings = #'("Ped." "*Ped." "*")
    
    % grace destroys pedal-line-spanner?
    % let's do manual tweak:
    \once\property Dynamics.SustainPedal \set #'extra-offset = #'(10 . 0) %tweak
    s8\sustainUp
    \once\property Dynamics.SustainPedal \set #'extra-offset = #'(16 . 0) %tweak
    s8\sustainDown
%{
    s4
    \grace {
  \property Voice.Stem \override #'stroke-style = #"grace"
  
    	   s8
    	   s16 s s
    	   s32 s s s\sustainUp
     	   s32 s s s\sustainDown
    
  \property Voice.Stem \revert #'stroke-style }

%}
    s2

    | %5
    % ugh, I don't think that 'mixed should show last edge, but rather:
    %
    %   Ped__________/\__________ *
    %
    % that's what gray wants, anyway.
    
    \property Dynamics.pedalSustainStyle = #'mixed
    s8\sustainDown s s
    s s\sustainUp\sustainDown s
    s
    \once \property Dynamics.pedalSustainStyle = #'text
    s\sustainUp

    | %6
    \property Dynamics.pedalSustainStyle = #'mixed
    s8\sustainDown s s
    s s\sustainUp\sustainDown s
    s
    \once \property Dynamics.pedalSustainStyle = #'text
    s\sustainUp
    | %7
}

\score{
    \context PianoStaff <<
        \context Staff=treble <<
	    \treble
	    \trebleTwo
        >>
	\new Dynamics <<
	    \middleDynamics
	>>
        \context Staff=bass <<
	    \clef bass
	    \bass
	    \bassTwo
        >>
	\new Dynamics <<
	    \lowerDynamics
	>>
    >>
    \paper {
	\translator {
	    \ScoreContext
	    \remove Bar_number_engraver
        }
	\translator {
	    \type "Engraver_group_engraver"
	    \name Dynamics
	    \consists "Output_property_engraver"
	    minimumVerticalExtent = #'(-1 . 1)

	    pedalSustainStrings = #'("Ped." "*Ped." "*")
	    
	    \consists "Piano_pedal_engraver"
	    \consists "Script_engraver"
	    \consists "Dynamic_engraver"
	    \consists "Text_engraver"

	    TextScript \override #'font-relative-size = #1
	    TextScript \override #'font-shape = #'italic

	    \consists "Skip_event_swallow_translator"

	    \consistsend "Axis_group_engraver"
    	}

	\translator {
	    \PianoStaffContext
	    \accepts Dynamics
	    VerticalAlignment \override #'forced-distance = #7
        }
    }
}

%%% Local variables:
%%% LilyPond-indent-level:4
%%% End:
%% new-chords-done %%
