#(ly:set-option 'old-relative)
\version "2.3.2"
\encoding "latin1"
\header {
    composer = "ARTHUR GRAY"
    title = "LES NÉRÉIDES"
    subtitle = "THE NEREIDS"
    enteredby = "JCN"
    piece = "Allegretto scherzando"
    copyright = "public domain"
}

%{

Nastiest piece of competition at
http://www.orphee.com/comparison/study.html, see
http://www.orphee.com/comparison/gray.pdf


lines that contain tweaks (17 currently, not counting reverts) are
marked with %tweak

possibly more impressive to render without tweaks?

  grep -v tweak input/les-nereides.ly >> lnnt.ly
  lilypond lnnt.ly

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

    \change Staff=bass

    \once\override TextScript  #'extra-offset = #'(-3 . -4) %tweak
    
    cis,16^2(^\markup {\small \italic "m.d." }\sustainUp
    <fis fis,>8 <e! e,!>
    | %4
    <dis, a' dis>4)\sustainDown
    
    \change Staff=treble
    
    \slurUp
    \set PianoStaff.connectArpeggios = ##t

    #(set-octavation 1)

    \once\override TextScript  #'extra-offset = #'(-3 . -2) %tweak
	    
    \tieUp
    cis''''4^\markup { \small \italic "m.g." }\arpeggio~
    \grace {
        cis8
	\slurBoth % Tweak
	
	\override Stem  #'direction = #0
	 
	a16[-5( fis dis]
	#(set-octavation 0)
	
	cis32[ a-1 fis-4 dis]   cis[ a  fis)-2]
				% the small grace in lower staff comes after us
	s32
    }


    \stemUp
    cis'4( bis)

    | %5
    r8 <a' a,>8(\mf <gis gis,> <fis fis,>
    
    % \fingerUp
    \override Fingering  #'direction = #1
    
    % Manual fix for collision with slur
    \override Fingering  #'extra-offset = #'(0 . 1) %tweak
    <gis gis,> <fis fis,> e)-1-4-5 r

    | %6
    r <a a,>8(\mf <gis gis,> <fis fis,>
    <gis gis,> <fis fis,>  e) r
    
    | %7
    \bar "||"
}

trebleTwo = \new Voice \notes\relative c''{
    \stemDown
    \slurDown
    % \fingerDown
    \override Fingering  #'direction = #-1
    \override Fingering  #'extra-offset = #'(0 . 1.2)
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

bass = \new Voice \notes\relative c{
    \partial 2
    \key a \major
    
    % Allow ugly (highly blown-up) slurs
    \override Slur  #'beautiful = #5.0 %tweak
    \override Slur  #'attachment-offset = #'((0 . 3) . (0 . -4))  %tweak
    \slurDown
    
    \dynamicUp

    r8. e,16(\f_2 <a a,>8[ <b b,>]
    | %2
    \override Staff.SustainPedalLineSpanner #'staff-padding = #5
			   
    <cis cis,>4\sustainDown
    \change Staff=treble
    \stemDown
    \override Slur  #'attachment = #'(stem . stem) %tweak
    <a'' eis cis>4)\arpeggio
    
    \revert Slur #'attachment %tweak
    \change Staff=bass
    \stemBoth
    
    \revert Slur #'y-free %tweak
    \override Slur  #'y-free = #0.1 %tweak
    \revert Slur #'attachment-offset %tweak
    \override Slur  #'attachment-offset = #'((0 . 3) . (0 . 8)) %tweak
    r8. cis,,16(\sustainUp <fis fis,>8 <gis gis,>
    
    | %3
    \override Stem  #'length = #5 %tweak
    <a a,>4\sustainDown
    \change Staff=treble
			    
    \revert Stem #'length %tweak
    \revert Stem #'direction
    \override Stem  #'direction = #-1
    <a' fis cis>)\arpeggio
    \change Staff=bass
    \revert Stem #'direction
    r2
    
    | %4
    \revert Slur #'beautiful %tweak
    \revert Slur #'attachment-offset %tweak
    \stemDown
    <b,, b,>4
    \clef treble
    \stemBoth
    <<
        %urg: staff-change: ! on dis
        <cis'' a fis dis!>\arpeggio
    >>
    
    \grace {
	\override Stem  #'stroke-style = #"grace"
  
        s8
        s16 s s
 	s32 s s
	s s s
	\clef bass
	<e,,, e,>32(\sustainUp\sustainDown
    
	\revert Stem #'stroke-style
    }
    <gis' e>2)
    
    | %5
    \slurUp
    
    % \fingerDown
    \override Fingering  #'direction = #-1

			   
    \override Staff.SustainPedalLineSpanner #'staff-padding = #3.5
    \set Staff.pedalSustainStyle = #'mixed
    %%a,8 e'[-5(<a-2 cis-3>])

			   
    a,8\sustainDown e'[-5(<a cis>])-2-3
    %%r b,-5 <e-3 gis-5 d'>4
    r b,-5\sustainUp\sustainDown <e gis d'>4-3-5
    \slurBoth
    \once \override Fingering  #'extra-offset = #'(0 . -1) %tweak
    e,8[-5(\sustainUp
    
    | %6
    \once \override Fingering  #'extra-offset = #'(0 . -1) %tweak
    a)-2]\sustainDown
    \slurUp
    e'[(<a cis>)] r b,\sustainUp\sustainDown <e gis d'>4
    \slurBoth
    e,8[(\sustainUp
    
    | %7
    a)]
}

bassTwo = \new Voice \notes\relative c{
    \skip 2
    \skip 1*2
    \skip 2

    \stemUp
    \slurUp

    cis'4( bis)
}

middleDynamics = \notes{
    \override Dynamics.TextScript  #'padding = #-1 %tweak
    s2
    s1*2
    | %4
    s2
    \grace {
	s8
	s16 s s
	s32 s
	\once\override Dynamics.Hairpin  #'extra-offset = #'(0 . 2) %tweak
	s\> s
	s32 s s s\!
    
    }

    s32 s-"rall." s s s8 s4
    | %5
    s2-"a tempo" s8
    \once\override Dynamics.Hairpin  #'extra-offset = #'(1 . 0) %tweak
    s \> s s
    | %6 
    s8\!
    s2 s8 s\> s
    | %7
    s8\!
}

theScore = \score{
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
    >>
    \paper {
	\context {
	    \Score
	    pedalSustainStrings = #'("Ped." "*Ped." "*")
	    \remove Bar_number_engraver
        }
	\context {
	    \type "Engraver_group_engraver"
	    \name Dynamics
	    \consists "Output_property_engraver"
	    minimumVerticalExtent = #'(-1 . 1)

	    \consists "Script_engraver"
	    \consists "Dynamic_engraver"
	    \consists "Text_engraver"

	    \override TextScript #'font-size = #2
	    \override TextScript #'font-shape = #'italic

	    \consists "Skip_event_swallow_translator"

	    \consistsend "Axis_group_engraver"
    	}

	\context {
	    \PianoStaff
	    \accepts Dynamics
	    \override VerticalAlignment #'forced-distance = #7
        }
    }
}
			   
\book{
    \score { \theScore }
}
    
%%% Local variables:
%%% LilyPond-indent-level:4
%%% End:
%% new-chords-done %%
