% scsii-menuetto.ly
%
% Solo Cello Suites
% J. S. Bach
%
% Suite II part v
% Menuetto I
%
% Copyright (c) 1995,1996,1997 Jan Nieuwenhuizen <jan@digicash.com>
% 
% The purpose of this file is to demonstrate features of LilyPond.
% (there is an accompanying LaTeX file, scsii-menuetto.tex)
%

% \lefttitle{Menuetto}
% \tempo{Moderato}
% \metron{4=120}
%
% \meter{3/4}
% \key\F		% key is F ( f-major )
% \clef\alto		% use alto clef
% \duration8		% default duration is 'eight note
%
% \verb{\nobarnumbers}	% issue MusiXTeX command
% \barnumbering0
% \slope{30}		% Fool MusiXTeX into 30 / 10 steeper beam
% \stemlength3		% shorter stemlength
%		% because piece is set very tightly
%%1		% double comments are included in .tex file
ii_menuetto = music {
	$\clef"alto"
	\key {bes}
	\octave{}
	\duration{8}
	{ a2 f2 d2_"\f" } bes4-.  |
%%2
	{ [ bes^"1" e c_"2"_"4" }
%	(^ )
	a bes-. g-. ] a4-.^"4" |
%%3
%	{ d4 `bes4-.\stemup } stemup: 2 voices?
	{ d4 `bes4-. } g4-. [ f-. e-. ] |
%%4
%	{ f `a } (^ e ) d cis-. `b-. `a-. ]
%	[ f ( e ) d cis-. `b-. `a-. ]
	{ [ f `a } e d cis-. `b-. `a-. ]
%%5
	{ a2 f2 d2 } bes4-. | %   @bes4-. |
%%6
%	{  bes( e  }  ) a
	{ [ bes e } a bes-. g-. ]
	'c4-. | % ='c4-. |
%%7
	{ a4-. f4}
%	{ 'd4-.-\upbow f4 `bes4\stemdown }
	{ 'd4-.-\upbow f4 `bes4 }
%	{ 'e4-.-\downbow g4 `g4\stemdown } |
	{ 'e4-.-\downbow g4 `g4 } |
%%8
%	{ 'cis2.-\upbow e2. `a2.\stemdown } :|:%%sorry!!
	{ 'cis2.-\upbow e2. `a2. } \bar ":|:"%%!! sorry!
%%9
	\clef "violin"
%	{ 'e2 a2_f\stemdown }
	{ 'e2 a2_"\f" }
	[ 'd( 'e16 )'f16 ] |
%%10
%	[ { 'e( g } ) 'd 'cis_{ }_{ }_2 'e
	{ [ 'e g } 'd 'cis_"2" 'e a( )g ] |
%%11
%	{ a4\stemup( f2\stemdown } ) 'd4\stemup
%	{ 'cis4\stemup-. e4\stemdown }
	{ \multivoice \music {\stem{1} a4(\stem{1}) 'd4 'cis4-. } \music { \stem{-1} f2 e4 } } |
%%12
%	{ 'g^4 d\stemdown } (^  'f 'e ) 'f 'd^3(^ ) ='c ] |
	{ [ 'g^"4" d } 'f 'e 'f 'd^"3"( ) 'c ] |
% %13
%	{ bes2\stemup g4\stemdown } ( )  f4
%	{ 'c4\stemup e4\stemdown }
%	{ bes2 g4 } f4 { 'c4 e4 } |
	\clef "alto"% 
	{ \multivoice
		\music { \stem{1} bes2 'c4 }
		\music { \stem{-1} g4(\stem{-1}) f4 e4 }% ugh
	} |
%%%14
%%	[ { a( f } ) g a f ] { 'd4-\upbow e4 `bes4\stemdown } |
	{ [ a f } g a f ] { 'd4-\upbow e4 `bes4 } |
%%%15
%%	{ 'c4-\downbow f4 `a4\stemdown } [ bes( )a g a ] |
%	{ 'c4-\downbow f4 `a4 } [ bes( )a g a ] |
%%%16
	[ f( e )f a-. g-. bes-. ] |
%% \tighten		% use one line less
%% \newline		% start new line
%%	|
%%%17
%%	{ a2^0^\tr fis2_3 } bes4 |
	{ a2^"0" fis2_"3" } bes4 |
%%%18
%%	[ { 'c( es } ) bes 'c a ] fis4^\tr |
	{ \music{ [ 'c( )bes 'c a ] } \music{ [ es ] } } fis4 |
%%%19
	{ 'd4-\downbow g4 `bes4 } { 'c4-\upbow g4 c4 } { [ bes d } a ] |
%%%20
%%	[ { 'c( d `g } bes a ) bes g ( ) bes ] |
	{ \music{ [ 'c( bes a )bes g( )bes ] } \music{ [ d ] } \music{ [ g ] } } |
%%%21
%%	{ 'd4\stemup g2\stemdown } (^ ) 'cis4\stemup { 'd4\stemup =f4\stemdown } |
	{ \multivoice \music {\stem{1}  'd4(\stem{1} ) 'cis4 'd4 } \music { \stem{-1} g2 f4 } } |
%%%22
%%	[ { g(  cis } )f g e ] { f4 d4 } |
	{ \music{ [ g( )f g e ] } \music{ [ cis ] } } { f4 d4 } |
%%%23
%%	[ `g g ] { e4.\stemup^\tr `a4.\stemdown } d\stemup-\upbow |
	[ `g g ] { e4. `a4. } d-\upbow |
%%%24
%%	{ d2.^{fine}  `a2. `d2.\stemup_{ }_{ }_{3 mins.}}	s4 :||
%%	{ \textstyle "italic" d2.^"fine" `a2. \textstyle "roman" `d2._"3 mins."} | % :||
	{ d2.^"fine" `a2. `d2._"3 mins."} \bar ":|" % \bar ":||"
%% \tighten		% use one line less
	$
}

score {
	staff { 
		melodic music { ii_menuetto }
	}
	commands {
		meter {3 * 4}
	}
	paper {
		symboltables { table_sixteen }
		width 195mm
		unitspace 9mm  % to get lily to use only three lines
		geometric 1.4
		output "scsii-menuetto.out"
	}
	midi { 
		tempo 4:120
		output "scsii-menuetto.midi"
	}
}
