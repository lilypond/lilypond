\header{
enteredby = 	 "jcn";
copyright = 	 "public domain";
TestedFeatures = 	 "This file tests Feta embedded slurs" +
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\version "1.3.117";

shortlong =  \notes{
	c4()c( c c  |
	c c c c |
	c c c c |
	\break
	c c c )c |
}

dirs =  \notes\transpose c' {

	c'1() g' () c () g () c |
}

complex =  \notes{
\transpose c'{
	c,16( e,( g,( b,( d( f( a( c'())))))))c4 c4 |}
\transpose c'''{
	c,16( a( f( d( b,( g,( e,( c,())))))))c4 c4 |}
}

over =  \notes\transpose c'{


	e( g' g' )e
	e( a' a' )e
	e( b' b' )e
	e( c'' c'' )e
	\stemUp
	e( c' c' )e
	e( d' d' )e
	e( e' e' )e
	e( c'' c'' )e
	\stemBoth
	e( f' b' )e
	e( b' f' )e
	e( e'' g' )e
	e( g' e'' )e
	\stemUp
	e( d' b )e
	e( b d' )e
	e( f' c'' )e
	e( c'' f )e
}

under =  \notes\transpose c'{
	\stemDown 
	f'( \stemBoth d d \stemDown )f'
	f'( \stemBoth c c \stemDown )f'
	f'( \stemBoth b, b, \stemDown )f'
	f'( \stemBoth a, a, \stemDown )f'
	\stemDown
	f'( a a )f'
	f'( g g )f'
	f'( f f )f'
	f'( a, a, )f'

	\stemDown 
	f'( \stemBoth d b, \stemDown )f'
	f'( \stemBoth b, d \stemDown )f'
	f'( \stemBoth d, b, \stemDown )f'
	f'( \stemBoth b, d, \stemDown )f'
	\stemDown
	f'( f a )f'
	f'( a f )f'
	f'( f e, )f'
	f'( e, f )f'
}

eccentric =  \notes\transpose c'{
	\stemUp
	\times 4/7 { f( a' f f f f )f } |
	\times 4/7 { f( f f f f a' )f } |
	\stemDown
	\times 4/7 { e'( c e' e' e' e' )e' } |
	\times 4/7 { e'( e' e' e' e' c )e' } |
}

tiltup =  \notes\transpose c'{
	e( c'' c'' )e'
	\stemUp
	e( c'' c'' )e'
	\stemBoth
	e( g' e'' )e'
	\stemUp
	e( c'' f )e'
	\stemDown 
	f'( \stemBoth a, a, \stemDown )f''
	\stemDown
	f'( a, a, )f''
}

tiltdown =  \notes\transpose c'{
	e'( c'' c'' )e
	\stemUp
	e'( c'' c'' )e
	\stemBoth
	e'( g' e'' )e
	\stemUp
	e'( c'' f )e
	\stemDown 
	f''( \stemBoth a, a, \stemDown )f'
	\stemDown
	f''( a, a, )f'
}

broken =  \notes\transpose c''{
      c c c c()
      c c c c(
      c )c c c(
      c c )c c(
      )a' a' a' a'()
      a' a' a' a'()
      c( c c )c 
      c( c c )f, 
      f,( c c )c 
      f,( c c )f,
}

blend = 	\notes\transpose c'{
	e( c'' c'' )e
	\stemUp
	f'( c'' c'' )f'
	e( c'' c'' )e
	\stemDown
	f'( \stemBoth a, a, \stemDown )f'
	f'( a, a, )f'

	\stemBoth
	e( c'' c'' )e'
	e'( c'' c'' )e
	d( d d )d

	\stemDown
	e( c'' c'' )e'
	e'( c'' c'' )e
	d( d d )d
}


bug =  \notes\transpose c'{

	a()g( f )e
	b'()a'( g' )f'
	g( f' e' )d'
	f f( g )a
	c' () b () c' c'
}

clipping =  \notes\transpose c'{
	\stemBoth
	c( c''' c''')c
	c( c''' c )c''
	c''( c c''' )c
	\stemDown
	c( \stemUp c,,, c,,, \stemDown )c
	c( \stemUp c,,, c \stemDown )c,,
	c,,( \stemUp c c,,, \stemDown )c
}

\score{
	\context Staff \notes{ 
%		\property Voice.pletvisibility = 0;
		% use blend for fast check
		\blend
% {
		\shortlong
		\dirs
		\complex
		\over
		\under
		\eccentric
		\tiltup
		\tiltdown
		\bug
% }
		\clipping
		% use broken with small linewidth
		%\broken
	}
	\paper{ 
	      indent = 0.0\pt;
		%for broken!
		% linewidth= 30.\mm;
	}
}

