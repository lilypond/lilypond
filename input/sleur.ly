\header{
enteredby =	 "jcn";
copyright =	 "PD";
TestedFeatures =	 "This file tests Feta embedded slurs"
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\version "0.1.9";

shortlong = \melodic{
	c4()c( c c  |
	c c c c |
	c c c c |
	c c c )c |
}

dirs = \melodic {
	\octave c';
	c'1() g' () c () g () c |
}

complex = \melodic{
	\octave c';
	'c16( 'e( 'g( 'b( d( f( a( c'())))))))c4 c4 |
	\octave c''';
	'c16( a( f( d( 'b( 'g( 'e( 'c())))))))c4 c4 |
}

over = \melodic{
	\octave c'; 

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

under = \melodic{
	\octave c'; 
	\stemdown 
	f'( \stemboth d d \stemdown )f'
	f'( \stemboth c c \stemdown )f'
	f'( \stemboth 'b 'b \stemdown )f'
	f'( \stemboth 'a 'a \stemdown )f'
	\stemdown
	f'( a a )f'
	f'( g g )f'
	f'( f f )f'
	f'( 'a 'a )f'

	\stemdown 
	f'( \stemboth d 'b \stemdown )f'
	f'( \stemboth 'b d \stemdown )f'
	f'( \stemboth 'd 'b \stemdown )f'
	f'( \stemboth 'b 'd \stemdown )f'
	\stemdown
	f'( f a )f'
	f'( a f )f'
	f'( f 'e )f'
	f'( 'e f )f'
}

eccentric = \melodic{
	\octave c';
	\stemup
	\[4/7 f( a' f f f f )f \] |
	\[4/7 f( f f f f a' )f \] |
	\stemdown
	\[4/7 e'( c e' e' e' e' )e' \] |
	\[4/7 e'( e' e' e' e' c )e' \] |
}

tiltup = \melodic{
	\octave c'; 
	e( c'' c'' )e'
	\stemup
	e( c'' c'' )e'
	\stemboth
	e( g' e'' )e'
	\stemup
	e( c'' f )e'
	\stemdown 
	f'( \stemboth 'a 'a \stemdown )f''
	\stemdown
	f'( 'a 'a )f''
}

tiltdown = \melodic{
	\octave c'; 
	e'( c'' c'' )e
	\stemup
	e'( c'' c'' )e
	\stemboth
	e'( g' e'' )e
	\stemup
	e'( c'' f )e
	\stemdown 
	f''( \stemboth 'a 'a \stemdown )f'
	\stemdown
	f''( 'a 'a )f'
}

broken = \melodic{
      \octave c'';
      c c c c()
      c c c c(
      c )c c c(
      c c )c c(
      )a' a' a' a'()
      a' a' a' a'()
      c( c c )c 
      c( c c )'f 
      'f( c c )c 
      'f( c c )'f
}

blend =	\melodic{
	\octave c';
	e( c'' c'' )e
	\stemup
	e( c'' c'' )e
	\stemdown
	f'( \stemboth 'a 'a \stemdown )f'
	f'( 'a 'a )f'

	\stemboth
	e( c'' c'' )e'
	e'( c'' c'' )e
	d( d d )d

	\stemdown
	e( c'' c'' )e'
	e'( c'' c'' )e
	d( d d )d
}


bug = \melodic{
	\octave c';
	a()g( f )e
	b'()a'( g' )f'
	g( f' e' )d'
	f f( g )a
	c' () b () c' c'
}

\score{
	\melodic{ 
		% use blend for fast check
		%\blend
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
		% use broken with small linewidth
		%\broken
	}
	\paper{ 
	      indent = 0.0\pt;
		%for broken!
		%linewidth= 40.\mm;
		%castingalgorithm = \Wordwrap;
	}
}

