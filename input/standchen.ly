%{
standchen.ly

 St\"andchen (Serenade) "Leise flehen meine Lieder" D.957 No.4
 Franz Schubert (1797-1828)
 Text by Ludwig Rellstab (1799-1860)

 Copyright (c) 1995,1996,1997 Jan Nieuwenhuizen <jandigicash.com>
 
 The purpose of this file is to demonstrate features of LilyPond.
 (there is an accompanying LaTeX file, standchen.tex)

%}
\version "0.0.57";

commands = \melodic{
	\skip 2.*4;
	\bar ":|";
	\skip 2.*24;
%	volta1
	\skip 2.*8;
%	volta2
	\bar ":|";
	\skip 2.*22;
	\bar "|."; 
	}

melodie = \melodic{ 
	\meter 3/4;
	\clef\violin;
	\key bes;
	\octave c';
	\duration 8;
	r \pp < [ d 'a-| > f-| < d 'a-| > f-| < d 'a-| ] > |
%%2
	r < [ d 'bes-| > f-| < d 'bes-| > f-| < d 'bes-| ] > |
%%3
	\textstyle "italic";
	r_"simile" < [ d 'bes > e < d 'bes > e < d 'bes ] >|
	\textstyle "roman";
	
%%4
	r < [ 'a cis > e < 'a cis > e < 'a cis ] > |
%%5
	[ a~ bes a ]2/3 d'4. a |
%%6
	[ g~ a g ]2/3 d'4 g r |

%%7
	a4.^> g [ g~ f e ]2/3 |
%%8
	f2 r4 |
%%9

	< { a'4.~  g' [ g'( f' )e' ]2/3 }
	{ cis'4. e'_"dolce" \plet 2/3; e' \plet 1/1; } > |
%%10
	< f'2. d'2. > |
%%11
	[ a ~  bes a ]2/3 f'4. a |
%%12
	[ g~   a g ]2/3 e'4. d' |
%%13
	c'4. bes [ bes~   a g ]2/3 |
%%14
	a2 r 
	< { d'~  c'4. g [ bes a g ]2/3 }
	{ f \p ~  e4._"dolce" bes 
		\plet 2/3; g
		\plet 1/1; 
	} > |
%%16
	< a2. f2. > |
%%17
	\textstyle "italic";
	[ a8._"cresc." cis'16 ] f'4. e'
	\textstyle "roman";|
%%18
	[ d'8. a16 ] f4. d |
%%19
%#%	c'\grace\stemup
	[ bes ~   a bes ]2/3 d'4. bes |
%%20
	a2. |
%%21
%#%	a\grace
	[ g~  fis g ]2/3 bes4.^> g |
%%22
	f!2. |
%%23
	[ a8. \mf cis'16 ] f'4. e' |
%%24
	[ d'8. a16 ] fis4. d |
%%25
	[ b \mf~   ais b ]2/3 d'4. b |
%%26
	< a2. fis2. > |
%%27
	[ e' \f~  dis' e' ]2/3 g'4. cis' |
%%28
	< d'2. fis2. > |
%#%\volta1
%%29
	< { bes2( [ d'8.~ )g16 ] }
	{ g2 \mf [ bes8. bes16 ] } > |
%%30
	< { a4. [ a-. a-. a-. ] }
	{ fis4. [ fis-.( fis-. )fis-. ] } > |
%%31 
	< { a4. [ a-. a-. a-. ] }
	{ g4. [ cis-.( e-. )g-. ] } > |
%%32
	< a2 fis2 \pp > < a4 fis4 > |
%%33
	< { b2( [ d'8.~  g16 ] }
	{ g2 [ b8. b16 ] } > |
%%34
	< { a4. [ a-. a-. a-. ] }
	{ fis4. [ fis-.( fis-. )fis-. ] } > |
%%35
	< { a4. [ a-. a-. a-. ] }
	{ g4. [ cis-.( e-. )g-. ] } > |
%%36
	< a2. fis2. > |
%#%\volta2
%%37
	[ a8. a16 ] [ cis'8. cis'16 ] [ e'8. e'16 ] |
%%38
	d'4~  cis'4 r4 |
%%39
%#%	> a4. [ cis' e'8. >! d'16 ] |
	a4. \> [ cis' e'8. \! d'16 ] |
%%40
	cis'2 r4 |
%%41
	< fis'4. \> cis'4. \f > e' [ e'~  d' \! cis' ]2/3 |
%%42
	[ b8. cis'16 ] d'4^> b r |
%%43

	[ b8. \f cis'16 ] d'4^> b r |
%%44
	[ b8. \> cis'16 ] d'4^> b \! r |
%%45
	[ b \p ~  ais b ]2/3 d'4. b |
%%46
	<a!2. fis2. > |
%%47
	[ e' \f~  dis' e' ]2/3 g'4.^> cis' |
%%48
	\textstyle "italic";
	< 
	{ fis2.~  \group "+1"; \stem -1; f2. }
	{ d'2. ~ \stem 1; d'4 r4_"decresc." d'4 } 
	>
	\textstyle "roman"; |
%%50
	< bes2. e2. > |
%%51
	< a2. cis2. > |
%%52
	< fis2 d2 > < a4 fis4 \pp > |
%%53

	< bes2 g2 > < [ d'8.^> bes8. > < bes16 g16 ] > |
%%54
	< { a4. [ a-. a-. a-. ] }
	{ fis4. [ fis-.( fis-. )fis-.] } > |
%%55
	< { a4. [ a-. a-. a-. ] }
	{ g4. [ cis-.( e-. )g-. ] } > |
%%56
	\textstyle "italic";
	< a2. fis2._"dim." >
	\textstyle "roman"; |
%%57
	< a2. fis2. > |
%%58
	< a2.^\fermata fis2. > |
%#%\tighten
}

begeleiding = \melodic{ 
	\meter 3/4;
	\clef\bass;
	\key bes;
	\octave c';
	\duration 2;
	'd r4 |
%%2
	''bes r4 |

%%3
	''g r4 |

%%4
	''a r4 |
%%5
\duration 8;
 	< \multivoice { \stem 1;	
		[ 'f 'a d 'a d 'a ] 
		[ 'd 'e 'g 'e 'g 'e ] 
		[ 'cis 'e 'g 'e 'g e ]
		[ 'd 'a d 'a d 'a ]
		}
	{ \stem -1; 
		'd2 r4 
		 ''bes2 r4 
		 ''a2 r4
		 'd2 r4 	
	} >
%%9
	[ 'a e g e g e ] |
%%10
	[ 'd 'a d 'a d 'a ] |
%%11
	< \multivoice 	
	{ \stem 1;
		[ 'f 'a d 'a d 'a ]
		[ 'd 'e 'g 'e 'g 'e ] 
		[ 'e 'g 'bes 'g 'bes 'g ] 
		[ 'a c f c f c ]
	} { \stem -1; 
		'd2 r4 
		 ''bes2 r4
		 'c2 r4 
		 'f2 r4 } 
	>
%%15
	[ 'c 'g 'bes 'g 'bes 'g ] |
%%16
	[ ''f 'c 'f 'c 'f 'c ] |
	< \multivoice
	{ \stem 1; 
		[ ''a 'e 'g 'e 'g 'e ][ 'd 'a d 'a d 'a ]
		[ ''bes 'f 'bes 'f 'bes 'f ][ ''f 'c 'f 'c 'f 'c ] }
	{ \stem -1; 
		''a2 r4
		'd2 r4 
		''bes2 r4 
		''f2 r4 
	} > 
%%21
	< [ 'e 'c > 'g c 'g c 'g ] |
%%22
	[ 'f 'a c 'a 'f 'c ] |
	< \multivoice {
		\stem 1;
		[ ''a 'e 'g 'e 'g 'e ] 
		[ 'd 'fis 'a 'fis 'a 'fis ] 
		[ ''g 'd 'b 'd 'b 'd ] 
		[ 'd 'a d 'a d 'a ] 
	}
	{\stem -1;
		''a2 r4 
		'd2 r4 
		''g2 r4 
		'd2 r4 
	}
	>
	< [ 'cis ''a > 'e 'a 'e 'a 'e ] |
%%28
	[ 'd 'a d 'a d 'a ] |
%%29
	[ 'd 'g 'bes 'g 'bes 'g ] |
%#%\volta1
%%30 
	[ 'd 'fis 'a 'fis 'a 'fis ] |
%%31
	[ ''a 'e 'a 'e 'a 'e ] |
%%32
	[ 'd 'fis 'a 'fis 'a 'fis ] |
%%33
	[ 'd 'g 'b 'g 'b 'g ] |
%%34
	[ 'd 'fis 'a 'fis 'a 'fis ] |
%%35
	[ ''a 'e 'a 'e 'a 'e ] |
%%36
	[ 'd 'fis 'a 'fis 'a 'fis ] |
%#%\volta2
%%37
	[ ''a 'e 'g 'e ''bes^> 'e ] |
%%38
	[ ''a < e cis 'a > < e cis 'a > < e cis 'a > < e cis 'a > < e cis 'a ] > |
%%39
	[ ''a 'e 'g 'e ''bes^> 'e ] |
%%40
	[ ''a < e cis 'a > < e cis 'a > < e cis 'a > < e cis 'a > < e cis 'a ] > |
%%41
	[ ''ais 'e 'fis 'e 'fis 'e ] |
%%42
	< [ 'd ''b > 'fis 'b 'fis 'b 'fis ] |
%%43
	< [ 'e ''b > 'g 'b 'g 'b 'g ] |
%%44
	< [ 'd ''b > 'fis 'b 'fis 'b 'fis ] |
%%45
	< \multivoice { \stem 1; 
		[ ''g 'd 'b 'd 'b 'd ] 
		[ 'd 'a d 'a d 'a ] 
	} {
		\stem -1;
		''g2 r4
		'd2 r4 
	} >

%%47
	< [ 'cis ''a > 'e 'a 'e 'a 'e ] |
%%48
	[ 'd 'fis 'a 'fis 'a 'fis ] |
%%49
	[ 'd 'a d 'a d 'a ] |
%%50
 	[ ''g 'e 'g 'e 'g 'e ] |
%%51
	[ ''a 'e 'g 'e 'g 'e ] |
%%52
	[ ''d 'd 'fis 'd 'fis 'd ] |
%%53
	[ 'd 'g 'bes 'g 'bes 'g ] |
%%54
	[ 'd 'fis 'a 'fis 'a 'fis ] |
%%55
	[ ''a 'e 'g 'e 'g 'e ] |
%%56
	[ ''d ''a 'd ''a 'd ''a ] |
%%57
	[ ''d ''a 'd ''a 'd ''a ]
%%58
	< 'd2.^\fermata ''d2. > |
 }


tekstI = \lyric{
	\meter 3/4;
	\duration 4;
	_ _ _
	_ _ _
	_ _ _
	_ _ _
% 5
	[ Lei- se8 ]2/3 fleh-4. en8 
	[ mei- ne8 ]2/3 Lie- der8 _8
	Durch4. die8 [ Nacht zu8 ]2/3 
	dir;2 _    
	_ _ _ 
	_ _ _
% 11
	[ In den8 ]2/3 stil-4. len8 
	[ Hain her-8 ]2/3 nie-4. der,8
	Lieb4. chen,8 [ komm zu8 ]2/3 
	mir!2 _
	_ _ _ 
	_ _ _

% 17
	Fl\"us-8. ternd16 schlan-4. ke8 
	Wip-8. fel16 rau-4. schen8
	[ In des8 ]2/3 Mon-4. des8 
	Licht;2.
	_ _ _ 
	_ _ _

% 23
	Des8. Ver-16 r\"a-4. ters8 
	feind-8. lich16 Lau-4. schen8
	[ F\"urch- te,8 ]2/3 Hol-4. de,8 
	nicht.2.
	_ _ _ 
	_ _ _

% volta 1
% 29
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _

% volta 2
% 37
	La\ss8. auch16 dir8. die16 Brust8. be-16
	we- gen, _
	Lieb-4. chen,8 h\"o-8. re16 
	mich!2 _
	Be-8. bend16 harr' ich8 _8
	dir8. ent-16 ge- gen!8 _8
	[ Komm, be-8 ]2/3 gl\"u4. cke8 
	mich!2.
	_ _ _ _ _ _
% 47
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
% 57
	_ _ _ _ _ _ 
	
}

tekstII = \lyric{
    \meter 3/4;
	\duration 4;
	_ _ _
	_ _ _
	_ _ _
	_ _ _
% 5
	[ H\"orst die8 ]2/3 Nach-4. ti-8 
	[ gal- len8 ]2/3 schla- gen?8 _8
	Ach!4. sie8 [ fleh- en8 ]2/3 
	dich,2 _
	_ _ _ 
	_ _ _

% 11
	[ Mit der8 ]2/3 T\"o-4. ne8
	[ s\"u\ss- en8 ]2/3 Kla-4. gen8
	Fleh-4. en8 [ sie f\"ur8 ]2/3
	mich.2 _
	_ _ _ 
	_ _ _

% 17
	Sie-8. ver-16 stehn4. des8
	Bus-8. ens16 Seh-4. nen,8
	[ Ken- nen8 ]2/3 Lieb-4. es-8 
	schmerz,2.
	_ _ _ 
	_ _ _

% 23
	R\"uh-8. ren16 mit4. den8 
	Sil-8. ber-16 t\"o-4. nen8
	[ Jed- es8 ]2/3 wei-4. che8 
	Herz.2.
	_ _ _ 
	_ _ _
% volta 1
% 29
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _

% volta 2
% 37
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _ 
	_ _ _ _ _ _
% 47
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
	_ _ _ _ _ _
% 57
	_ _ _ _ _ _ 
	
}

\score{
	\staff{ lyricregs tekstI }
	\staff{ lyricregs tekstII }

	\staff{ melodicregs melodie commands}
	\staff{ melodicregs begeleiding commands }
	\paper{
		\width 195\mm

		% on two pages...
		\unitspace 9\mm
		\geometric 1.2
		\output "standchen.out"
	}
	\midi{
		\tempo 4:54
	}
}
