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
%% Still looks fairly on the "ugh" side the beauty scale, mainly because of
%% LilyPond's naive idealspacing calc.
%%

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
melodie = music { $ 
	\duration{ 8 }
%	r_"\pp" { [ d `a-| } f-| { d `a-| } f-| { d `a-| ] }
	r { [ d `a-|_"\pp" } f-| { d `a-| } f-| { d `a-| ] }
%%2
	r { [ d `bes-| } f-| { d `bes-| } f-| { d `bes-| ] }
%%3
	\textstyle "italic"
	r_"simile" { [ d `bes } e { d `bes } e { d `bes ] }
	\textstyle "roman"
%%4
	r { [ `a cis } e { `a cis } e { `a cis ] }
%#%:|
%%5
	\plet{ 2/3 } [ a()bes a ] \plet{ 1/1 } 'd4. a
%%6
	\plet{ 2/3 } [ g()a g ] \plet{ 1/1 } 'd4 a r
%#%\tighten
%#%\newline
%%7
	a4.^> g \plet{ 2/3 } [ g( f )e ] \plet{ 1/1 }
%%8
	f2 r4
%%9
%#%	{ 'a4.( 'cis4.\stemdown_p } { ) 'g 'e_{dolce} }
%#%	{ 'g( 'e } 'f ) 'e
	'a4.( )'g \plet{ 2/3 } { 'g 'e } { 'f } { 'e } \plet{ 1/1 }
%%10
	{ 'f2. 'd2. }
%%11
	\plet { 2/3 } [ a_"\pp"( )bes a ] \plet { 1/1 } 'f4. a
%%12
	\plet { 2/3 } [ g( ) a g ] \plet { 1/1 } 'e4. 'd
%%13
	'c4. bes \plet { 2/3 } [ bes( ) a g ] \plet { 1/1 }
%%14
%#%	a2 r { 'd(^ f_p(v }
	a2 r { 'd f_"\p" }
%%15
%#%	{ ) e4. ) 'c4.\stemup_{dolce} } { bes g }
	{ e4. 'c4._"dolce" } { bes g }
	\plet { 2/3 } { [ bes g } a g ] \plet { 1/1 }
%%16
	{ a2. f2. }
%%17
	\textstyle "italic"
	[ a8._"cresc." 'cis16 ] 'f4. 'e
	\textstyle "roman"
%%18
	[ 'd8. a16 ] f4. d
%%19
%#%	'c\grace\stemup
	\plet { 2/3 } [ bes_"\pp"( ) a bes ] \plet { 1/1 } 'd4. bes
%%20
	a2.
%%21
%#%	a\grace
	\plet { 2/3 } [ g( )fis g ] \plet { 1/1 } bes4.^> g
%%22
%#%	=f2.
	f2.
%%23
	[ a8._"\mf" 'cis16 ] 'f4. 'e
%%24
	[ 'd8. a16 ] fis4. d
%%25
%#%	'cis\grace\stemup
	\plet { 2/3 } [ b_"\mf"( ) ais b ] \plet { 1/1 }	'd4. b
%%26
	{ a2. fis2. }
%#%\tighten\newline
%%27
	\plet { 2/3 } [ 'e_"\f"( )'dis 'e ] \plet { 1/1 } 'g4. 'cis
%%28
	{ 'd2. fis2. }
%#%\volta1
%%29
%#%	{ bes2(^++++ g2_{mf} } 
	{ bes2 g2_"\mf" } 
%#%	[v { 'd8.( bes8. } { bes16 ) )++ g16 } ]
	{ [ 'd8. bes8. } { bes16 g16 ] }
%%30
	{ a4. fis4. } 
%#%	[ { [ a_. f_.( } { a_. f_. } { a_. ) f_. } ]
	{ [ a_. f_. } { a_. f_. } { a_. f_. ] }
%%31  
	{ a4. g4. }
%#%	[ { a cis_.( } { a e_. } { a )- g_. } ]
	{ [ a cis_. } { a e_. } { a g_. ] }
%%32
	{ a2 fis2_"\pp" } { a4 fis4 }
%%33
%#%	{ b2(^ g2 } [v { 'd8.( b8. } { b16 ) ) g16 } ]
	{ b2 g2 } { [ 'd8. b8. } { b16 g16 ] }
%%34
%#%	{ a4. fis4. } [ { a_. fis_.( } { a_. fis_. } { a_. ) fis_. } ]
	{ a4. fis4. } { [ a_. fis_. } { a_. fis_. } { a_. fis_. ] }
%%35
%#%	{ a4. g4. } [ { a cis_.( } { a e_. } { a )- g_. } ]
	{ a4. g4. } { [ a cis_. } { a e_. } { a g_. ] }
%%36
	{ a2. fis2. }
%#%\volta2
%#%:|
%%37
	[ a8. a16 ] [ 'cis8. 'cis16 ] [ 'e8. 'e16 ]
%%38
	'd4( )'cis4 r4
%%39
%#%	> a4. [ 'cis 'e8. >! 'd16 ]
	a4. [ 'cis 'e8. 'd16 ]
%%40
	'cis2 r4
%%41
%#%	{ > 'fis4. 'cis4._f } 'e [ 'e( ) 'd >! 'cis ] \plet { 1/1 }
	{ 'fis4. 'cis4._"\f" } 'e \plet { 2/3 } [ 'e( )'d 'cis ] \plet { 1/1 }
%%42
	[ b8. 'cis16 ] 'd4^> b r
%%43
%#%	[^ { b8._{ }_{ }_{ }_{ }_{ }_f g2. } 'cis16 ] 'd4^> b r
	[ b8._"\f" 'cis16 ] 'd4^> b r
%%44
%#%	[^ { > b8. fis2 } 'cis16 ] 'd4^> b >! r
	[ b8. 'cis16 ] 'd4^> b r
%%45
%#%	'cis\grace\stemup
	\plet { 2/3 } [ b_"\p"( )ais b ] \plet { 1/1 } 'd4. b
%%46
%#%	{ =a2. fis2. }
	{ a2. fis2. }
%%47
	\plet { 2/3 } [ 'e_"\f"( )'dis 'e ] \plet { 1/1 } 'g4.^> 'cis
%%48
%#%	{ fis2.(v ='d2.\stemup(^ }
	{ fis2. 'd2. }
%%49
%#%	{ ) 'd4 ) =f2._> } { r4 s4_{decr} } 'd4\stemup
	\textstyle "italic"
	'd4 r4_"decresc." 'd4
	\textstyle "roman"
%%50
	{ bes2. e2. }
%%51
	{ a2. cis2. }
%%52
	{ fis2 d2 } { a4 f4_"\pp" }
%%53
%#%	{ bes2 g2 } [ { 'd8.^>( bes8. } { ) bes16 g16 } ]
	{ bes2 g2 } { [ 'd8.^> bes8. } { bes16 g16 ] }
%%54
%#%	{ a4. fis4. } [ { a_. fis_.( } { a_. fis_. } { a_. ) fis_. } ]
	{ a4. fis4. } { [ a_. fis_. } { a_. fis_. } { a_. fis_. ] }
%%55
%#%	{ a4. g4. } [ { a cis_.( } { a e_. } { a )- g_. } ] 
	{ a4. g4. } { [ a cis_. } { a e_. } { a g_. ] }
%%56
	\textstyle "italic"
	{ a2. fis2._"dim." }
	\textstyle "roman"
%%57
	{ a2. fis2. }
%%58
	{ a2.^\fermata fis2. }
%#%\tighten
$ }

begeleiding = music { $
	\duration{2}
	`d r4
%%2
	``bes r4

%%3
	``g r4

%%4
	``a r4
%#%:|
%%5
%#%\stemlength2
\duration{ 8 }
%#%	{ =`f `d2 } `a d `a { d r4 } `a
% i-d rather type:
% 	{ \music{ [ `f `a d `a d `a ] } \music{ `d2 r4 } }
% output little better...
% 	{ `d2 \music{ [ `f `a d `a } } { \music{ d `a ] } r4 }
 	[ `f `a d `a d `a ]
%%6
%#%	{ `d ``bes2 } `e `g `e { `g r4 } `e
	[ `d `e `g `e `g `e ]
%%7
%#%	{ `cis ``a2 } `e `g `e { `g r4 } `e
	[ `cis `e `g `e `g `e ]
%%8
%#%	{ `d `d2 } `a d `a { d r4 } `a
	[ `d `a d `a d `a ]
%%9
%#%	`a e f e f e
	[ `a e f e f e ]
%%10
%#%	`d `a d `a d `a
	[ `d `a d `a d `a ]
%%11
%#%	[^ { `f `d2 } `a d `a { d r4 } `a ]
	[ `f `a d `a d `a ]

%%12 == 6
%#%	{ `d ``bes2 } `e `g `e { `g r4 } `e
	[ `d `e `g `e `g `e ]
%13
%#%	{ `e ``bes2 } `e `g `e { `g r4 } `e
	[ `e `e `g `e `g `e ]
%%14
%#%	{ `a `f2 } c f c { f r4 } c
	[ `a c f c f c ]
%%15
	[ `c `g `bes `g `bes `g ]
%%16
	[ ``f `c `f `c `f `c ]
%%17
%#%	[^ { ``a ``a2 } `e `g `e { `g r4 } `e ]
	[ ``a `e `g `e `g `e ]
%%18
%#%	[^ { `d `d2 } `a d `a { d r4 } `a ]
	[ `d `a d `a d `a ]
%%19
%#%	s8		 % skip space of `grace note
%#%	[^ { ``bes ``bes2 } `f `bes `f { `bes r4 } `f ]
	[ ``bes `f `bes `f `bes `f ]
%%20
%#%	[^ { ``f ``f2 } `c `f `c { `f r4 } `c ]
	[ ``f `c `f `c `f `c ]
%%21
%#%	s8		 % skip space of `grace note
%#%	[ { `e `c } `g c `g c `g ]
	{ [ `e `c } `g c `g c `g ]
%%22
	[ `f `a c `a `f `c ]
%%23
%#%	[^ { ``a ``a2 } `e `g `e { `g r4 } `e ]
	[ ``a `e `g `e `g `e ]
%%24
%#%	[^ { `d `d2 } `fis `a `fis { `a r4 } `fis]
	[ `d `fis `a `fis `a `fis ]
%%25
%#%	s8		 % skip space of `grace note
%#%	[^ { ``g ``g2 } `d `b `d { `b r4 } `d]
	[ ``g `d `b `d `b `d ]
%%26
%#%	[^ { `d `d2 } `a d `a { d r4 } `a]
	[ `d `a d `a d `a ]
%%27
	{ [ `cis ``a } `e `a `e `a `e ]
%%28
	[ `d `a d `a d `a ]
%%29
%#%	[ `d `g @ `bes `g `bes `g	]
	[ `d `g `bes `g `bes `g ]
%#%\volta1
%%30 
	[ `d `fis `a `fis `a `fis ]
%%31
	[ `a `e `a `e `a `e ]
%%32
	[ `d `fis `a `fis `a `fis ]
%%33
	[ `d `g `b `g `b `g ]
%%34
	[ `d `fis `a `fis `a `fis ]
%%35
	[ `a `e `a `e `a `e ]
%%36
	[ `d `fis `a `fis `a `fis ]
%#%\volta2
%#%:|
%%37
	[ `a `e `g `e ``bes^> `e ]
%%38
	[ `a { e cis `a } { e cis `a } { e cis `a } { e cis `a } { e cis `a ] } 
%%39
	[ `a `e `g `e ``bes^> `e ]
%%40
	[ `a { e cis `a } { e cis `a } { e cis `a } { e cis `a } { e cis `a ] } 
%%41
	[ `ais `e `gis `e `gis `e ]
%%42
	{ [ `d ``b } `fis `b `fis `b `fis ]
%%43
	{ [ `e ``b } `g `b `g `b `g ]
%%44
	{ [ `d ``b } `fis `b `fis `b `fis ]
%%45
%#%	s8		 % skip space of `grace note
%#%	[^{ ``g ``g2 } `d `b `d { `b r4 } `d]
	[ ``g `d `b `d `b `d ]
%%46
%#%	[^ { `d `d2 } `a d `a { d r4 } `a ]
	[ `d `a d `a d `a ]
%%47
	{ [ `cis ``a } `e `a `e `a `e ]
%%48
	[ `d `fis `a `fis `a `fis ]
%%49
	[ `d `a d `a d `a ]
%%50
 	[ ``g `e `g `e `g `e ]
%%51
	[ `a `e `g `e `g `e ]
%%52
	[ ``d `d `fis `d `fis `d ]
%%53
	[ `d `g `bes `g `bes `g ]
%%54
	[ `d `fis `a `fis `a `fis ]
%%55
	[ `a `e `g `e `g `e ]
%%56
	[ ``d ``a `d ``a `d ``a ]
%%57
	[ ``d ``a `d ``a `d ``a ]
%%58
	{ `d2.^\fermata ``d2. }
$ }


tekst1 = music {
	@
	\duration{4}
	_ _ _
	_ _ _
	_ _ _
	_ _ _
% 5
	\plet{ 2/3 } Lei- se8 \plet{ 1/1 } fleh-4. en8 
	\plet{ 2/3 } mei- ne8 \plet{ 1/1 } Lie- der8 _8
	Durch4. die8 \plet{ 2/3 } Nacht zu8 \plet{ 1/1 } 
	dir;2 _        
	_ _ _ 
	_ _ _
% 11
	\plet{ 2/3 } In den8 \plet{ 1/1 } stil-4. len8 
	\plet{ 2/3 } Hain her-8 \plet{ 1/1 } nie-4. der,8
	Lieb4. chen,8 \plet{ 2/3 } komm zu8 \plet{ 1/1 } 
	mir!2 _
	_ _ _ 
	_ _ _

% 17
	Fl\"us-8. ternd16 schlan-4. ke8 
	Wip-8. fel16 rau-4. schen8
	\plet{ 2/3 } In des8 \plet{ 1/1 } Mon-4. des8 
	Licht;2.
	_ _ _ 
	_ _ _

% 23
	Des8. Ver-16 r\"a-4. ters8 
	feind-8. lich16 Lau-4. schen8
	\plet{ 2/3 } F\"urch- te,8 \plet{ 1/1 } Hol-4. de,8 
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
	\plet{ 2/3 } Komm, be-8 \plet{ 1/1 } gl\"u4. cke8 
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
	\plet{ 2/3 } H\"orst die8 \plet{ 1/1 } Nach-4. ti-8 
	\plet{ 2/3 } gal- len8 \plet{ 1/1 } schla- gen?8 _8
	Ach!4. sie8 \plet{ 2/3 } fleh- en8 \plet{ 1/1 } 
	dich,2 _
	_ _ _ 
	_ _ _

% 11
	\plet{ 2/3 } Mit der8 \plet{ 1/1 } T\"o-4. ne8
	\plet{ 2/3 } s\"u\ss- en8 \plet{ 1/1 } Kla-4. gen8
	Fleh-4. en8 \plet{ 2/3 } sie f\"ur8 \plet{ 1/1 }
	mich.2 _
	_ _ _ 
	_ _ _

% 17 ok tot hier
	Sie-8. ver-16 stehn4. des8
	Bus-8. ens16 Seh-4. nen,8
	\plet{ 2/3 } Ken- nen8 \plet{ 1/1 } Lieb-4. es-8 
	schmerz,2.
	_ _ _ 
	_ _ _

% 23
	R\"uh-8. ren16 mit4. den8 
	Sil-8. ber-16 t\"o-4. nen8
	\plet{ 2/3 } Jed- es8 \plet{ 1/1 } wei-4. che8 
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
	staff { 
		lyric 
		music { tekst1 }
		music { tekst2 }
	}
	staff { 
	 	melodic 
		music { melodie }
		commands {
			key $bes$
			clef "violin"
		}
	}
	staff { 
		melodic 
		music { begeleiding }
		commands {
			key $bes$
			clef "bass"
		}
	}
	commands { 
		meter 3*4 
		skip 4:0
		bar ":|"
		skip 32:0
		bar ":|"
		skip 22:0
		bar "||"
	}
	paper {
		symboltables { table_sixteen }
		width 195mm
		unitspace 20mm
%		unitspace 4.0 cm % leaves all text stand free
%		geometric 1.4
		output "standchen.out"
	}
}
