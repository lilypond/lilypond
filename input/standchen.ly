% standchen.ly
%
% St\"andchen (Serenade) "Leise flehen meine Lieder" D.957 No.4
% Franz Schubert (1797-1828)
% Text by Ludwig Rellstab (1799-1860)
%
% Copyright (c) 1995,1996,1997 Jan Nieuwenhuizen <jan@digicash.com>
% 
% The purpose of this file is to demonstrate features of LilyPond.
% (there is an accompanying LaTeX file, standchen.tex)
%
% comments with # note missing stuff
% heu, make that %#%
%
% thanks to \music group changing, all notes 
% (except for grace notes) are printed
%

%#%%
%#%\title{St\"andchen}
%#%\subtitle{Serenade}
%#%%
%#%\composer{Franz Schubert}
%#%\tempo{M\"a\ss ig}
%#%%
%#%\meter{3/4}
%#%\key\F
%#%\clef\treble
%#%%
%#%\barnumbering5
%#%%\staffnumbers
%#%\barnumberstyle\boxed
%#%%
%#%\staffseparation{12}
%#%%
%#%\duration8
%%1
melodie = music { $ \clef\violin
	\key{bes}
	\duration{ 8 }
	r_"\pp" { [ d `a-| } f-| { d `a-| } f-| { d `a-| ] } |
%%2
	r { [ d `bes-| } f-| { d `bes-| } f-| { d `bes-| ] } |
%%3
	\textstyle "italic"
	r_"simile" { [ d `bes } e { d `bes } e { d `bes ] }
	\textstyle "roman" |
%%4
	r { [ `a cis } e { `a cis } e { `a cis ] } |
%#%:|
%%5
	[ a()bes a ]2/3  'd4. a |
%%6
	[ g()a g ]2/3  'd4 a r |
%#%\tighten
%#%\newline
%%7
	a4.^> g  [ g( f )e ]2/3  |
%%8
	f2 r4 |
%%9
%	{ \music{ 'a4.( )'g  [ 'g( 'f )'e ]2/3  } \music{ 'cis4. 'e_"dolce"  [ 'e ] 2/3 } } |
	{ \music{ 'a4.( )'g  [ 'g( 'f )'e ]2/3  } \music{ 'cis4. 'e_"dolce"  \plet{ 2/3 } 'e \plet{ 1/1 } } } |
%%10
	{ 'f2. 'd2. } |
%%11
	[ a_"\pp"( )bes a ]2/3  'f4. a |
%%12
	[ g( ) a g ]2/3  'e4. 'd |
%%13
	'c4. bes  [ bes( ) a g ]2/3  |
%%14
	a2 r 
%	{ \music{ 'd( | )'c4. g  [ bes a g ]2/3  } \music{ f_"\p"( | )e4._"dolce" bes  [ g ]2/3  } } |
	{ \music{ 'd( | )'c4. g  [ bes a g ]2/3  } \music{ f_"\p"( | )e4._"dolce" bes  \plet{ 2/3 } g \plet{ 1/1 }  } } |
%%16
	{ a2. f2. } |
%%17
	\textstyle "italic"
	[ a8._"cresc." 'cis16 ] 'f4. 'e
	\textstyle "roman" |
%%18
	[ 'd8. a16 ] f4. d |
%%19
%#%	'c\grace\stemup
	[ bes_"\pp"( ) a bes ]2/3  'd4. bes |
%%20
	a2. |
%%21
%#%	a\grace
	[ g( )fis g ]2/3  bes4.^> g |
%%22
%#%	=f2. |
	f2. |
%%23
	[ a8._"\mf" 'cis16 ] 'f4. 'e |
%%24
	[ 'd8. a16 ] fis4. d |
%%25
%#%	'cis\grace\stemup
	[ b_"\mf"( ) ais b ]2/3 'd4. b |
%%26
	{ a2. fis2. } |
%#%\tighten\newline
%%27
	[ 'e_"\f"( )'dis 'e ]2/3 'g4. 'cis |
%%28
	{ 'd2. fis2. } |
%#%\volta1
%%29
	{ \music { bes2( [ 'd8.( ))g16 ] } \music{ g2_"\mf" [ bes8. bes16 ] } } |
%%30
	{ \music{ a4. [ a-. a-. a-. ] } \music{ fis4. [ f-.( f-. )f-. ] } } |
%%31  
	{ \music{ a4. [ a-. a-. a-. ] } \music{ g4. [ cis-.( e-. )g-. ] } } |
%%32
	{ a2 fis2_"\pp" } { a4 fis4 } |
%%33
%#%	{ b2(^ g2 } [v { 'd8.( b8. } { b16 ) ) g16 } ] |
	{ \music{ b2( [ 'd8.( ) )g16 ] } \music{ g2 [ b8. b16 ] } } |
%%34
%#%	{ a4. fis4. } [ { a-. fis-.( } { a-. fis-. } { a-. ) fis-. } ] |
%	{ a4. fis4. } { [ a-. fis-. } { a-. fis-. } { a-. fis-. ] } |
	{ \music{ a4. [ a-. a-. a-. ] } \music{ fis4. [ fis-.( fis-. )fis-. ] } } |
%%35
%#%	{ a4. g4. } [ { a cis-.( } { a e-. } { a )- g-. } ] |
%	{ a4. g4. } { [ a cis-. } { a e-. } { a g-. ] } |
	{ \music{ a4. [ a-. a-. a-. ] } \music{ g4. [ cis-.( e-. )g-. ] } } |
%%36
	{ a2. fis2. } |
%#%\volta2
%#%:|
%%37
	[ a8. a16 ] [ 'cis8. 'cis16 ] [ 'e8. 'e16 ] |
%%38
	'd4( )'cis4 r4 |
%%39
%#%	> a4. [ 'cis 'e8. >! 'd16 ] |
	a4. [ 'cis 'e8. 'd16 ] |
%%40
	'cis2 r4 |
%%41
%#%	{ > 'fis4. 'cis4._f } 'e [ 'e( ) 'd >! 'cis ]  |
	{ 'fis4. 'cis4._"\f" } 'e  [ 'e( )'d 'cis ]2/3  |
%%42
	[ b8. 'cis16 ] 'd4^> b r |
%%43
%#%	[^ { b8._{ }_{ }_{ }_{ }_{ }_f g2. } 'cis16 ] 'd4^> b r |
	[ b8._"\f" 'cis16 ] 'd4^> b r |
%%44
%#%	[^ { > b8. fis2 } 'cis16 ] 'd4^> b >! r |
	[ b8. 'cis16 ] 'd4^> b r |
%%45
%#%	'cis\grace\stemup
	[ b_"\p"( )ais b ]2/3  'd4. b |
%%46
%#%	{ =a2. fis2. } |
	{ a2. fis2. } |
%%47
	[ 'e_"\f"( )'dis 'e ]2/3  'g4.^> 'cis |
%%48
%#%	{ fis2.(v ='d2.\stemup(^ }
%	{ fis2. 'd2. } |
%%49
%#%	{ ) 'd4 ) =f2._> } { r4 s4_{decr} } 'd4\stemup
	\textstyle "italic"
	{ \multivoice \music{ \stem{ 1 } fis2.( | \stem{ -1 } )f2. } \music { \stem{ 1 } 'd2.( | \stem{ 1 } )'d4 r4_"decresc." 'd4 } }
	\textstyle "roman" |
%%50
	{ bes2. e2. } |
%%51
	{ a2. cis2. } |
%%52
	{ fis2 d2 } { a4 f4_"\pp" } |
%%53
%#%	{ bes2 g2 } [ { 'd8.^>( bes8. } { ) bes16 g16 } ] |
	{ bes2 g2 } { [ 'd8.^> bes8. } { bes16 g16 ] } |
%%54
%#%	{ a4. fis4. } [ { a-. fis-.( } { a-. fis-. } { a-. ) fis-. } ] |
%	{ a4. fis4. } { [ a-. fis-. } { a-. fis-. } { a-. fis-. ] } |
	{ \music{ a4. [ a-. a-. a-. ] } \music{ fis4. [ fis-.( fis-. )fis-.] } } |
%%55
%#%	{ a4. g4. } [ { a cis-.( } { a e-. } { a )- g-. } ]  |
%	{ a4. g4. } { [ a cis-. } { a e-. } { a g-. ] } |
	{ \music{ a4. [ a-. a-. a-. ] } \music{ g4. [ cis-.( e-. )g-. ] } } |
%%56
	\textstyle "italic"
	{ a2. fis2._"dim." }
	\textstyle "roman" |
%%57
	{ a2. fis2. } |
%%58
	{ a2.^\fermata fis2. } |
%#%\tighten
$ }

begeleiding = music { $
	\key{bes}
	\clef\bass
	\duration{2}
	`d r4 |
%%2
	``bes r4 |

%%3
	``g r4 |

%%4
	``a r4 |
%#%:|
%%5
%#%\stemlength2
\duration{ 8 }
%#%	{ =`f `d2 } `a d `a { d r4 } `a
 	{ \multivoice \music{ \stem{ 1 } [ `f `a d `a d `a ] } \music { \stem{ -1 } `d2 r4 } } |
%%6
 	{ \multivoice \music{ \stem{ 1 } [ `d `e `g `e `g `e ] } \music { \stem{ -1 } ``bes2 r4 } } |
%%7 
% this (one note missing) fails with assertion:
% lilypond: src/beam.cc:144: void Beam::set_grouping(struct Rhythmic_grouping, struct Rhythmic_grouping): Assertion `cur.children.size() == stems.size()' failed.
%	{ \multivoice \music{ \stem{ 1 } [ `cis `e `g `e `g ] } \music { \stem{ -1 } ``a2 r4 } } |
	{ \multivoice \music{ \stem{ 1 } [ `cis `e `g `e `g e ] } \music { \stem{ -1 } ``a2 r4 } } |
%%8
	{ \multivoice \music{ \stem{ 1 } [ `d `a d `a d `a ] } \music { \stem{ -1 } `d2 r4 } } |
%%9
	[ `a e f e f e ] |
%%10
	[ `d `a d `a d `a ] |
%%11
	{ \multivoice \music{ \stem{ 1 } [ `f `a d `a d `a ] } \music { \stem{ -1 } `d2 r4 } } |
%%12 == 6
	{ \multivoice \music{ \stem{ 1 } [ `d `e `g `e `g `e ] } \music { \stem{ -1 } ``bes2 r4 } } |
%13
	{ \multivoice \music{ [ `e `e `g `e `g `e ] } \music { \stem{ -1 } ``bes2 r4 } } |
%%14
	{ \multivoice \music{ \stem{ 1 } [ `a c f c f c ] } \music { \stem{ -1 } `f2 r4 } } |
%%15
	[ `c `g `bes `g `bes `g ] |
%%16
	[ ``f `c `f `c `f `c ] |
%%17
	{ \multivoice \music{ \stem{ 1 } [ ``a `e `g `e `g `e ] } \music { \stem{ -1 } ``a2 r4 } } |
%%18
	{ \multivoice \music{ \stem{ 1 } [ `d `a d `a d `a ] } \music { \stem{ -1 } `d2 r4 } } |
%%19
	{ \multivoice \music{ \stem{ 1 } [ ``bes `f `bes `f `bes `f ] } \music { \stem{ -1 } ``bes2 r4 } } |
%%20
	{ \multivoice \music{ \stem{ 1 } [ ``f `c `f `c `f `c ] } \music { \stem{ -1 } ``f2 r4 } } |
%%21
%#%	s8		 % skip space of grace note
	{ [ `e `c } `g c `g c `g ] |
%%22
	[ `f `a c `a `f `c ] |
%%23
	{ \multivoice \music{ \stem{ 1 } [ ``a `e `g `e `g `e ] } \music { \stem{ -1 } ``a2 r4 } } |
%%24
	{ \multivoice \music{ \stem{ 1 } [ `d `fis `a `fis `a `fis ] } \music { \stem{ -1 } `d2 r4 } } |
%%25
%#%	s8		 % skip space of grace note
	{ \multivoice \music{ \stem{ 1 } [ ``g `d `b `d `b `d ] } \music { \stem{ -1 } ``g2 r4 } } |
%%26
	{ \multivoice \music{ \stem{ 1 } [ `d `a d `a d `a ] } \music { \stem{ -1 } `d2 r4 } } |
%%27
	{ [ `cis ``a } `e `a `e `a `e ] |
%%28
	[ `d `a d `a d `a ] |
%%29
%#%	[ `d `g @ `bes `g `bes `g ] |
	[ `d `g `bes `g `bes `g ] |
%#%\volta1
%%30 
	[ `d `fis `a `fis `a `fis ] |
%%31
	[ `a `e `a `e `a `e ] |
%%32
	[ `d `fis `a `fis `a `fis ] |
%%33
	[ `d `g `b `g `b `g ] |
%%34
	[ `d `fis `a `fis `a `fis ] |
%%35
	[ `a `e `a `e `a `e ] |
%%36
	[ `d `fis `a `fis `a `fis ] |
%#%\volta2
%#%:|
%%37
	[ `a `e `g `e ``bes^> `e ] |
%%38
	[ `a { e cis `a } { e cis `a } { e cis `a } { e cis `a } { e cis `a ] }  |
%%39
	[ `a `e `g `e ``bes^> `e ] |
%%40
	[ `a { e cis `a } { e cis `a } { e cis `a } { e cis `a } { e cis `a ] }  |
%%41
	[ `ais `e `gis `e `gis `e ] |
%%42
	{ [ `d ``b } `fis `b `fis `b `fis ] |
%%43
	{ [ `e ``b } `g `b `g `b `g ] |
%%44
	{ [ `d ``b } `fis `b `fis `b `fis ] |
%%45
%#%	s8		 % skip space of grace note
	{ \multivoice \music{ \stem{ 1 } [ ``g `d `b `d `b `d ] } \music { \stem{ -1 } ``g2 r4 } } |
%%46
	{ \multivoice \music{ \stem{ 1 } [ `d `a d `a d `a ] } \music { \stem{ -1 } `d2 r4 } } |
%%47
	{ [ `cis ``a } `e `a `e `a `e ] |
%%48
	[ `d `fis `a `fis `a `fis ] |
%%49
	[ `d `a d `a d `a ] |
%%50
 	[ ``g `e `g `e `g `e ] |
%%51
	[ `a `e `g `e `g `e ] |
%%52
	[ ``d `d `fis `d `fis `d ] |
%%53
	[ `d `g `bes `g `bes `g ] |
%%54
	[ `d `fis `a `fis `a `fis ] |
%%55
	[ `a `e `g `e `g `e ] |
%%56
	[ ``d ``a `d ``a `d ``a ] |
%%57
	[ ``d ``a `d ``a `d ``a ]
%%58
	{ `d2.^\fermata ``d2. } |
$ }


tekst1 = music {
	@
	\duration{4}
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
	Lieb-4. chen,8  h\"o-8. re16 
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
	@
}

tekst2 = music {
        @
	\duration{4}
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
%	_ _ _ _ _ _
%	_ _ _ _ _ _
%	_ _ _ _ _ _
%	_ _ _ _ _ _ 
%	_ _ _ _ _ _
% 47
%	_ _ _ _ _ _
%	_ _ _ _ _ _
%	_ _ _ _ _ _
%	_ _ _ _ _ _
%	_ _ _ _ _ _
% 57
%	_ _ _ _ _ _ 
	@
}

score {
%	staff { 
%		lyric 
	%	music { tekst1 }
	%	music { tekst2 }
%	}
%	staff { 
%	 	melodic 
%		music { melodie }
%	}
%	staff { 
%		melodic 
%		music { begeleiding }
%	}
	commands { 
		meter {3*4}
	}
	paper {
		symboltables { table_sixteen }
		width 195mm

		% on two pages...
%		unitspace 10mm
%		geometric 1.2
		unitspace 9mm
		geometric 1.2
		output "standchen.out"
	}
	staff { midi music { melodie } }
	staff { midi music { begeleiding } }
	midi {
		tempo 4:50
	}
}
