
% #(set! point-and-click line-column-location)

\header {
title = "Solo Cello Suite II"
piece ="Sarabande"
composer = "J.S.Bach"
editor = "August Wenzinger"
source= "B\\\"arenreiter Urtext"

texidoc = "The B\\\"arenreiter edition of the Cello Suites is the most
beautifully typeset piece of music in our collection of music (we both
own one. It is also lovely on French Horn). This piece follows the
same beaming as the printed edition. This is done in order to
benchmarkk the quality of the LilyPond output. As of lilypond 1.5.38,
the spacing is almost identical. With a line-break forced before
measure 25, we get back the linebreaking of Baerenreiter.

This file used to show spacing weaknesses. Now it shows weaknesses in
beam and slur handling.

Note that the Barenreiter edition contains a mistake. The second line
begins with measure 6, not 5.  "


}


\version "1.3.148"


sarabandeA =  \context Voice \notes \relative c {
    \property Staff.NoteCollision \set #'merge-differently-dotted = ##t
  	< { d8. e16 e4.-\trill d16 e } \\
	  { d4 a2 } >
	f4.  [e8 d c] |
	[bes g'] [f e16(f] [g a bes)d,] |
	cis4.-\trill b8 a g |

% check spacing without accs: 
%	c4.-\trill [bes8 a g] |
	
	< { d'8. e16 f4.-\trill d16 e |
	    f4. d8 e f }
	  \\
	  { <a,4 f> a2 <a4. d,4.>  } > |
	%5

	g8 bes16()a c()bes a()g d'8 f, |
	<  e4.-\trill
	   \\ <a,,4 e'> >
	  [d8 c bes]
	%8
	< { f'8 g16()a a4. g16()f  |
	     g8 a16()bes bes4. c16()d }
	  \\
  	  { a,4 <bes4. d4. > r8 bes4 <g2 f'2>  }
 	> |

	% 11
        [e,8 f] [c, g'] [f' e] |
	f4 f,2 |
	< {  a'4 a4.-\trill bes8 
	     c bes16 a } \\
	  { [f8 es] es4. r8 d4 } >

	fis8.-\trill es16 d8 c |
	[bes g'] [a, fis'] [es' d] |
	%16
	< bes4.-\trill d, g, > a8 g f! |
	e bes a f' g a |
	d, as g es' f g |
	[cis, bes'] [a g16 f] [e!8 f16 d] |
	cis8 e16 a a,8. g'16 f8()e |
	%21
	< { d e16()f f4. e16()d |
	    e8 f16()g g4. a16()bes |
	    a8 cis16 d d,8 e16 f32 g f8-\trill e16()d } \\
	  { bes4 g2 |
	    g4 <bes4. cis,> s8 |
	    <d8 a f> r r g, a4 } >
	|
	d4 d,16 a'( b cis d e f )g |
    \break
	%25
	< { a16(b c)b c4. b16()a |
	    b cis d cis d4. e16()f | }
	  \\
	  { f,4 fis4. s8 |
	    <d4 g,> gis4.   } >
	d16(cis)d f,  a,8 e' d' cis |
	d4 d,,2 |
}


sarabande =  \context Staff \notes<
	\apply #voicify-music \sarabandeA
	
>

\version "1.3.148"

sarabandeCelloGlobal =  \notes{
	\time 3/4
	\key f \major
	\clef bass
	\repeat "volta" 2 {
		s2.*12
	} \repeat "volta" 2 {
		s2.*16
	}
}

sarabandeCelloScripts =  \notes{
}

sarabandeCelloStaff =  \context Staff <
	\sarabande
	\sarabandeCelloGlobal
	\sarabandeCelloScripts
>

\score{
	\sarabandeCelloStaff
	\paper{
	    indent = 7. \mm
	    linewidth = 183.5 \mm
	\translator { \ScoreContext
%		SpacingSpanner \override #'maximum-duration-for-spacing = #(make-moment 1 16)


}}
	\midi{ \tempo 4 = 40 }
	\header{
	opus= "" 
	piece ="Sarabande" }
}

