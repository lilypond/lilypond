\header{
filename =	 "wohltemperirt.ly";
title =	 "Fuga a 3";
description =	 "Das Wohltemperierte Clavier I, Fuga II (c-minor)";
opus =	 "BWV 847-Fuga";
source =	 "Henle's Urtext";
composer =	 "Johann Sebastian Bach (1685-1750)";
enteredby =	 "HWN and Werner Lemberg";
copyright =	 "Public Domain";
}
%{
 Tested Features: stem direction, multivoice, forced accidentals.
%}

\version "0.1.8";

                                    % should add \need{dutch.ini} for
                                    % correct parsing of note names

global = 
    \melodic {
         \meter 4/4;                % should be \meter C
         \keyCminor 
	 \skip 1*31;
	 \bar "|."; |
    }
  
dux =
    \melodic {
        \clef "violin";
        \octave c';

        \stemdown
        r8 [c'16 b] [c'8 g] [as c'16 b] [c'8 d'] |
        [g c'16 b] [c'8 d'] [f16 g] as4 [g16 f] |
        [es c' b a] [g f! es d] [c8 es' d' c'] |
        [bes a bes c'] [fis g a fis] |
%% 5
        g4 r16 [c d es] [f g as8~] [as16 d es f] |
        [g a bes8~] [bes16 es f g] [as g f es] [d8 c'16 b!] |
                                                    % forced accident!
        c'4 r4 r8 [f' es' d'] |
        r8 [as g f] [g f16 es] [f8 d] | 
        g4 r8 b [c' c'16 b] [c'8 g] |
%% 10   
        as4 r8 a [bes bes16 a] [bes8 f] |
        g4 r8 g [as as g f] |
	\translator Staff=bass \stemup
        \octave c; r8 [as bes c'] r8 [as16 g] [as8 f8] |
        [bes8 c' bes as] [bes g f es] |
        [f des' c' bes] [c' as g f] |
%% 15
	\translator Staff=treble \stemdown
        g8 \octave c'; [g16 fis] [g8 c] [es g16 fis!] [g8 a] |
        [d g16 fis] [g8 a!] [c16 d] es4 [d16 c] |
                                                    % forced accident!
        'bes8 r8 r16 [d e fis] [g a bes8~] [bes16 e f g] |
        [a bes c'8~] [c'16 fis16 g a] [bes8 es!16 d] [es8 'g] |
        ['as f16 es] [f8 'a8] ['bes g16 f] [g8 'b] |
%% 20
        [c16 f es d] [c Bes! As G] [F8 as g f] |
        [es d es f] [B c d B] |
        c4 r8 e8 [f f16 e] [f8 c] |
        d4 r8 d8 [es8 es16 d] [es8 Bes] |
        c2 ~  [c8 d16 es][ f es f d] |
%% 25
        B8 r8 r B c r r es |
        d r r f~ f r r f |
        [es as g f] [es d es f] |
        [B c d B] [B c] r c |
        [f16 d es c]~ [c8 B] c4 r8 e |
%% 30
        f4 r8 f [f es16 d] [es8 <f as]> |
        <B d> r <B d> r <G2 c> |
    }


comes =
    \melodic {
        \octave c'';
        \stemup
        r1 |
        r1 |
        r8 [g16 fis] [g8 c] [es g16 fis] [g8 a] |
        [d8 g16 fis] [g8 a] [c16 d] es4 [d16 c] |
%% 5
        ['bes8 es16 d] [es8 'g8] ['as f16 es] [f8 'a] 
        ['bes8 g16 f] [g8 'b] [c8 d16 es] f4~ |
        [f8 es16 d] [c16 'bes! 'as 'g] ['f8 as g f] 
        [es d es f] ['b c d 'b] |
        [c g16 fis] [g8 d] es4 r8 e8 |
%% 10 
        [f f16 e] [f8 c8] d4 r8 d |
        [es8 es16 d] [es8 'bes] [c es16 d] [es8 f] |
        ['bes es16 d] [es8 f] ['as16 'bes] c4 ['bes16 'as] |
        [G16 Es F G] [As Bes c d] [es d c d] [es f g a] |
        [bes F G As] [Bes c d e] [f es d es] [ f g a b] |
%% 15
        [c'8 b16 a] [g f! es d] [c8 es d c] |
        [Bes A Bes c] [Fis! G A Fis] |
                                                    % forced accident
        [G8 d16 c] d8 r8 r8 [e16 d] e8 r8 |
        r [fis16 e] fis8 r r [G16 F] G8 r8 |
        r8 [A16 G] A8 r r [B16 A] B8 r |
%% 20
        r8 [c16 'b] [c8 'g] [As c16 'b] [c8 d] |
        [G c16 B] [c8 d] [F16 G] As4 [G16 F] |
        [Es8 c16 B] [c8 G] As4 r8 A |
        [Bes8 Bes16 A] [Bes8 F8] 'g4 r8 G~ |
        [G As16 Bes] [c B c As] F2~ |
%% 25
        [F8 d16 c] [d8 F] [Es es16 d] [es8 G] |
        [F f16 es] [f8 As] [G16 f es d] [c B A G] |
        [c8 f es d] r [As G F] |
        [G F16 Es] [F8 D] [As G] r A |
        [B c] [F16 Es D C] C8 [c16 B] [c8 G] |
%% 30
        [As c16 B] [c8 <d 'b! 'as!]> [G8 c16 B] [c8 d] |
        [F16 G] As4 [G16 F] E2 |
    }

bassdux =
    \melodic {
        \clef "bass";
        \octave c';
        r1 |
        r |
        r |
        r |
%% 5
        r |
        r1 |
        r8 [c16 B] [c8 G] [As c16 B] [c8 d] |
        [G c16 B] [c8 d] [F16 G] As4 [G16 F] | 
        \octave c; [es c' b a] [g f es d] [c d es d] [c Bes! As! G] |
                                                    % -> \classic_accidentals
%% 10
        [F bes as g] [f es d c] [Bes c d c] [Bes As G F] |
        [Es as g f] [es des c Bes] [As8 c' bes as] |
        [g8 f g as] [d es f d] |
        [es as g f] [g es d c] |
        [d bes as g] [as f es d!] |
%% 15
        es8 r8 r4 r8 [c Bes A] |
        r [es d c] [d c16 Bes] [c8 d] |
        [G8 bes16 a] [bes8 d] [es c'16 bes] [c'8 e] |
        [f d'16 c'] [d'8 fis] g4 r16 [G A B] |
        [c16 d es8~] [es16 A Bes c] [d es f8~] [f16 'b c d] |
%% 20    
        es8 r r e [f F Es! D] |                     % -> \classic_accidentals
        r [As G F] [G F16 Es] [F8 G] |
        [c16 d  es d] [c Bes As G] [F bes as g] [f es d c] |
        [Bes c d c] [Bes As G F] [Es as g f] [es d c Bes] |
        [As Bes c Bes] [As G F Es] [D g f es] [d c B A] |
%% 25
        G4 r4 r16 [G A B] [c d es f] |
        [g f as g] [f es d c] [B8 c16 B] [c8 G] |
        [As c16 B] [c8 d] [G c16 B] [c8 d] |
        [F16 G] As4 [G16 F] Es4 r8 es |
        [d c g G]
%% 30
        \multi 2 <
        	{ \stemup c2~ | c1~ | c1 }
	        { \stemdown C2~ | C1~ | C1 }
        >
    }

        
\score {
    \melodic \type Grandstaff < 
	\type Staff=treble  < 
                       \global 
                       \dux
                       \comes 
                 >
	\type Staff=bass  <
                       \global
                       \bassdux
                 >
    >

    \paper{
	gourlay_maxmeasures =5.;
	
    }
    \midi {
        \tempo 4 = 84;
    }
}

% EOF
