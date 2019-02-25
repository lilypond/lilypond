\version "2.21.0"

\header {
    composer = "ARTHUR GRAY"
    title = "LES NÉRÉIDES"
    subtitle = "THE NEREIDS"
    enteredby = "JCN"
    piece = "Allegretto scherzando"
    copyright = "public domain"

    texidoc="Highly tweaked example of lilypond output"
}

%{

Nastiest piece of competition at
http://www.orphee.com/comparison/study.html, see
http://www.orphee.com/comparison/gray.pdf

Lines that contain tweaks (3 currently, not counting reverts) are
marked with %tweak

possibly more impressive to render without tweaks?

  grep -v tweak input/les-nereides.ly >> lnnt.ly
  lilypond lnnt.ly

%}

treble = \new Voice \relative c''{
    \key a \major
    r2
    | %2
    \stemUp
    r4 <cis eis a cis>\arpeggio r2
    | %3
    r4 <cis fis a cis>\arpeggio r8.

    \change Staff=bass

    cis,16^2(^\markup {\small \italic "m.d." }\sustainOff
    <fis fis,>8 <e! e,!>
    | %4
    <dis, a' dis>4)\sustainOn

    \change Staff=treble

    \slurUp
    \set PianoStaff.connectArpeggios = ##t

    \ottava #1

    \tieUp
    cis''''4^\markup { \small \italic "m.g." }\arpeggio~
    \grace {
        cis8
	\revert Stem.direction

	a16[-5_( fis dis]
	\ottava #0

	cis32[ a-1 fis-4 dis]   cis[ a  fis)-2]
				% the small grace in lower staff comes after us
	s32
    }


    \stemUp
    cis'4( bis)

    | %5
    r8 <a' a,>8(\mf <gis gis,> <fis fis,>

    % \fingerUp
    \override Fingering.direction = #UP

    <gis gis,> <fis fis,> e)-1-4-5 r

    | %6
    r <a a,>8(\mf <gis gis,> <fis fis,>
    <gis gis,> <fis fis,>  e) r

    | %7
    \bar "||"
}

trebleTwo = \new Voice \relative c''{
    \stemDown
    \slurDown
    % \fingerDown
    \override Fingering.direction = #DOWN
    s2
    | %1
    s1*2
    | %4
    s4
    <cis' a fis dis>4\arpeggio
    <e, gis, e d!>2
    | %5
    s8 cis4. d4
    <cis e,>8[( <d,_3 b'_1>
    | %6
    <cis_1 a'_2>)] cis'4. d4
    <cis e,>8[( <b d,>
    | %7
    <a cis,>)]
}

bass = \new Voice \relative c{
    \partial 2
    \key a \major

    \slurDown
    \dynamicUp

    r8. e,16(\f_2 <a a,>8[ <b b,>]
    | %2
    %\override Staff.SustainPedalLineSpanner.staff-padding = #5 %tweak

    <cis cis,>4\sustainOn
    \change Staff=treble
    \stemDown
    <a'' eis cis>4)\arpeggio

    \change Staff=bass
    \stemNeutral

    r8. cis,,16(\sustainOff <fis fis,>8 <gis gis,>

    | %3

    <a a,>4\sustainOn
    \change Staff=treble

    \stemNeutral
    \stemDown
    <a' fis cis>)\arpeggio
    \change Staff=bass
    \stemNeutral
    r2

    | %4
    \stemDown
    <b,, b,>4
    \clef treble
    \stemNeutral
    <<
        %urg: staff-change: ! on dis
        <cis'' a fis dis!>\arpeggio
    >>

    \grace {
	\override Flag.stroke-style = "grace"

        s8
        s16 s s
 	s32 s s
	s s s
	\clef bass
	<e,,, e,>32(\sustainOff\sustainOn

	\revert Flag.stroke-style
    }
    <gis' e>2)

    | %5
    \slurUp

    % \fingerDown
    \override Fingering.direction = #DOWN


    %\override Staff.SustainPedalLineSpanner.staff-padding = #3.5 %tweak
    \set Staff.pedalSustainStyle = #'mixed
    %%a,8 e'[-5(<a-2 cis-3>])


    a,8\sustainOn e'[-5(<a cis>])-2-3
    %%r b,-5 <e-3 gis-5 d'>4
    r b,-5\sustainOff\sustainOn <e gis d'>4-3-5
    \slurNeutral
    e,8[-5(\sustainOff

    | %6
    a)-2]\sustainOn
    \slurUp
    e'[(<a cis>)] r b,\sustainOff\sustainOn <e gis d'>4
    \slurNeutral
    e,8[(\sustainOff

    | %7
    a)]
}

bassTwo = \new Voice \relative c{
    \skip 2
    \skip 1*2
    \skip 2

    \stemUp
    \slurUp

    cis'4( bis)
}

middleDynamics = {
    %\override Dynamics.TextScript.padding = #-1 %tweak
    s2
    s1*2
    | %4
    s2
    \grace {
	s8
	s16 s s
	s32 s
	s\> s
	s32 s s\! s

    }
    \textLengthOn
    s32 s-"rall." s s s8 s4
    \textLengthOff
    | %5
    s2-"a tempo" s8
    s \> s s
    | %6
    s8\!
    s2 s8 s\> s
    | %7
    s8\!
}

theScore = \score{
    \context PianoStaff <<
        \new Staff = "treble" <<
            \set beamExceptions =
	      \beamExceptions { 8[ 8] 8[ 8] 8[ 8] 8[ 8] |
				\repeat unfold 8 { 32[ 32 32 32] } }
	    \treble
	    \trebleTwo
        >>
	\new Dynamics <<
	    \middleDynamics
	>>
        \new Staff = "bass" <<
	    \clef bass
	    \bass
	    \bassTwo
        >>
    >>
    \layout {
	\context {
	    \Score
	    pedalSustainStrings = #'("Ped." "*Ped." "*")
	    \remove "Bar_number_engraver"
        }
    }
}

\book{
    \score { \theScore }
}

%%% Local variables:
%%% coding: utf-8
%%% LilyPond-indent-level:2
%%% End:

