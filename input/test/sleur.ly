\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests Feta embedded slurs"
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\version "1.0.2";

shortlong = \melodic{
	c4()c( c c  |
	c c c c |
	c c c c |
	\break
	c c c )c |
}

dirs = \melodic\transpose c' {

	c'1() g' () c () g () c |
}

complex = \melodic{
\transpose c'{
	c,16( e,( g,( b,( d( f( a( c'())))))))c4 c4 |}
\transpose c'''{
	c,16( a( f( d( b,( g,( e,( c,())))))))c4 c4 |}
}

over = \melodic\transpose c'{


	e( g' g' )e
	e( a' a' )e
	e( b' b' )e
	e( c'' c'' )e
	\stemup
	e( c' c' )e
	e( d' d' )e
	e( e' e' )e
	e( c'' c'' )e
	\stemboth
	e( f' b' )e
	e( b' f' )e
	e( e'' g' )e
	e( g' e'' )e
	\stemup
	e( d' b )e
	e( b d' )e
	e( f' c'' )e
	e( c'' f )e
}

under = \melodic\transpose c'{

	\stemdown 
	f'( \stemboth d d \stemdown )f'
	f'( \stemboth c c \stemdown )f'
	f'( \stemboth b, b, \stemdown )f'
	f'( \stemboth a, a, \stemdown )f'
	\stemdown
	f'( a a )f'
	f'( g g )f'
	f'( f f )f'
	f'( a, a, )f'

	\stemdown 
	f'( \stemboth d b, \stemdown )f'
	f'( \stemboth b, d \stemdown )f'
	f'( \stemboth d, b, \stemdown )f'
	f'( \stemboth b, d, \stemdown )f'
	\stemdown
	f'( f a )f'
	f'( a f )f'
	f'( f e, )f'
	f'( e, f )f'
}

eccentric = \melodic\transpose c'{

	\stemup
	\[4/7 f( a' f f f f )f \] |
	\[4/7 f( f f f f a' )f \] |
	\stemdown
	\[4/7 e'( c e' e' e' e' )e' \] |
	\[4/7 e'( e' e' e' e' c )e' \] |
}

tiltup = \melodic\transpose c'{
	e( c'' c'' )e'
	\stemup
	e( c'' c'' )e'
	\stemboth
	e( g' e'' )e'
	\stemup
	e( c'' f )e'
	\stemdown 
	f'( \stemboth a, a, \stemdown )f''
	\stemdown
	f'( a, a, )f''
}

tiltdown = \melodic\transpose c'{
	e'( c'' c'' )e
	\stemup
	e'( c'' c'' )e
	\stemboth
	e'( g' e'' )e
	\stemup
	e'( c'' f )e
	\stemdown 
	f''( \stemboth a, a, \stemdown )f'
	\stemdown
	f''( a, a, )f'
}

broken = \melodic\transpose c''{
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

blend =	\melodic\transpose c'{
	e( c'' c'' )e
	\stemup
	f'( c'' c'' )f'
	e( c'' c'' )e
	\stemdown
	f'( \stemboth a, a, \stemdown )f'
	f'( a, a, )f'

	\stemboth
	e( c'' c'' )e'
	e'( c'' c'' )e
	d( d d )d

	\stemdown
	e( c'' c'' )e'
	e'( c'' c'' )e
	d( d d )d
}


bug = \melodic\transpose c'{

	a()g( f )e
	b'()a'( g' )f'
	g( f' e' )d'
	f f( g )a
	c' () b () c' c'
}

clipping = \melodic\transpose c'{
	\stemboth
	c( c''' c''')c
	c( c''' c )c''
	c''( c c''' )c
	\stemdown
	c( \stemup c,,, c,,, \stemdown )c
	c( \stemup c,,, c \stemdown )c,,
	c,,( \stemup c c,,, \stemdown )c
}

\score{
	\melodic{ 
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
		castingalgorithm = \Wordwrap;
	}
}

