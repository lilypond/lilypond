\version "1.7.18"

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

%}

#(ly:set-point-and-click 'line-column)
#(define (make-text-checker text)
  (lambda (elt) (equal? text (ly:get-grob-property elt 'text))))


treble =  \context Voice=treble \notes\relative c''{
    \key a \major
    r2
    %2
    \stemUp
    r4 <<cis eis a cis>>-\arpeggio r2
    %3
    r4 <<cis fis a cis>>-\arpeggio r8.

    \translator Staff=bass

    \once\property Voice.TextScript \set #'extra-offset = #'(-3 . -4)
	    
    % currently, this can't be (small italic,-) because in the paperblock
    % we set italic_magnifictation to get large italics.
    cis,16^2^"m.d."( <<fis fis,>>8 <<e! e,!>>
    %4
    <<dis, a' dis>>4-)
    
    \translator Staff=treble
    
    \slurUp 
    \property PianoStaff.connectArpeggios = ##t

    #(set-octavation 1)

    \once\property Voice.TextScript \set #'extra-offset = #'(-3 . -2)
	    
    \tieUp
    cis''''4^\markup { \small \italic "m.g." }-\arpeggio-~
    \grace {
         cis8
	\stemBoth
          a16-[-5( fis dis-]
	 #(set-octavation 0)
	
 	 cis32-[ a-1 fis-4 dis]   cis-[ a  fis-)-2]
    }

    \stemUp
    cis'4( bis-)
    r8
    <<a' a,>>8-( <<gis gis,>> <<fis fis,>> <<gis gis,>> <<fis fis,>>  e-)^" "^1^4^5 r|
    r <<a a,>>8-( <<gis gis,>> <<fis fis,>> <<gis gis,>> <<fis fis,>>  e-) r|

    \bar "||"

}

trebleTwo =  \context Voice=trebleTwo \notes\relative c''{
    s2
    s1*2
    s4
    \stemDown
    <<cis' a fis dis>>4-\arpeggio

    <<e, gis, e d!>>2
    s8 cis4. d4
    <<cis e,>>8-[-( <<b-3 d,-1>> |
    <<a-2 cis,-1>>-)] cis4. d4 
    <<cis e,>>8-[-( <<b d,>> |
    <<a cis,>>-)]
}

bass =  \context Voice=bass \notes\relative c{
    % Allow ugly slurs
    \partial 2
    \key a \major
    
    \property Voice.Slur \override #'beautiful = #5.0
    \property Voice.Slur \override #'attachment-offset = #'((0 . 3) . (0 . -4))
    \slurDown
    r8. e,16_2( <<a a,>>8-[ <<b b,>>] |
    %2
    <<cis cis,>>4
    \translator Staff=treble
    \stemDown
    \property Voice.Slur \override #'attachment = #'(stem . stem)
    <<a'' eis cis>>4-)-\arpeggio 
    %\stemBoth
    \property Voice.Slur \revert #'attachment
    \translator Staff=bass
    \stemBoth
    \property Voice.Slur \revert #'y-free
    \property Voice.Slur \override #'y-free = #0.1
    \property Voice.Slur \revert #'attachment-offset
    \property Voice.Slur \override #'attachment-offset = #'((0 . 3) . (0 . 8))
    r8. cis,,16( <<fis fis,>>8 <<gis gis,>>
    %3
    \property Voice.Stem \set #'length = #5
    <<a a,>>4
    \translator Staff=treble
			    
    \property Voice.Stem \revert #'length
    \property Voice.Stem \revert #'direction
    \property Voice.Stem \override #'direction = #-1
    <<a' fis cis>>-)-\arpeggio
    \translator Staff=bass
    \property Voice.Stem \revert #'direction
    r2
    %4
    \property Voice.Slur \revert #'beautiful
    \property Voice.Slur \revert #'attachment-offset
    \stemDown
    <<b,, b,>>4
    \clef treble
    \stemBoth
    <
        %urg: staff-change: ! on dis
        <<cis'' a fis dis!>>-\arpeggio
%	{ s8. \clef bass}
    >

    %Hmm
    %\grace { s16 s s s s32 s s s s \clef bass s }
    \clef bass
    \grace { <<e,,, e,>>32-( } <<gis' e>>2-)
    %5
    \slurUp
    a,8   e'-[-5(<<a-2 cis-3>>-)]
    r b,-5 <<e-3 gis-5 d'>>4
    \slurBoth
      e,8-[-5(|
    %6
     a-)-2]
    \slurUp
      e'-[(<<a cis>>-)] r b, <<e gis d'>>4
    \slurBoth
      e,8-[(|
    %7
     a-)]
}

bassTwo =  \context Voice=bassTwo \notes\relative c{
    \skip 2
    \skip 1*2
    \skip 2

    \stemUp
    \slurUp

    cis'4( bis-)
}

middleDynamics =  \context Dynamics=middle \notes{
    s8 s16 s-\f s4
    s1*2
    %4
    s2

    s32 
    \once \property Dynamics.TextScript \set #'extra-offset = #'(0 . 1.5)
    s-"rall." s s
    s8 s4
    
    %s1-\mf-"a tempo"
    s2-\mf-"a tempo" s8
%%    s\>  s8-\!
    s16 s32 s64 \> s s8  s  s8-\!
%%    s8-\mf s4 s4 s8\> s32 s s  s-\!
    s8-\mf s4 s8 s16 s32 s64 \> s s16 s8 s32 s s s s128-\!
}

lowerDynamics =  \context Dynamics=lower \notes{
    s2
    %2
    s2-\sustainDown s8. s16-\sustainUp s4
    %3
    s2-\sustainDown s8. s16-\sustainUp s4
    %3

    s4-\sustainDown s16
    s32 s s-\sustainUp s
    s32-\sustainDown s s s
    s8

    \property Dynamics.pedalSustainStrings = #'("Ped." "*Ped." "")
    s4 s16. s32--\sustainUp

    %5
    s8-\sustainDown s s
    \property Dynamics.pedalSustainStrings = #'("Ped." "-P" "*")
    s s-\sustainUp-\sustainDown s s
    s-\sustainUp

    %6
    \property Dynamics.pedalSustainStrings = #'("Ped." "*Ped." "")
    s8-\sustainDown s s
    \property Dynamics.pedalSustainStrings = #'("Ped." "-P" "*")
    s s-\sustainUp-\sustainDown s s
    s-\sustainUp
}

\score{
    \context PianoStaff <
        \context Staff=treble <
	    \treble
	    \trebleTwo
        >
	\context Dynamics=middle <
	    \middleDynamics
	>
        \context Staff=bass <
	    \clef bass
	    \bass
	    \bassTwo
        >
	\context Dynamics=lower <
	    \lowerDynamics
	>
    >
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
	    pedalUnaCordaStrings = #'("una corda" "" "tre corde")
	    
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
