 \header {
 composer =  "Domenico Scarlatti";
 title="Sonata K.3";
 opus="L.378";
 movement="Presto";
 copyright = "General Public License";
}

tolower = \translator Staff = lower
toupper = \translator Staff = upper

RHtoL = { \tolower \stemdown }
RHtoR = { \toupper \stemup } 
LHtoR = { \toupper \stemup } 
LHtoL = { \tolower \stemdown } 

\score {
 \notes 
   \context PianoStaff  <
    \context Staff = upper <\context Voice = mainV <{
    % 1
	\stemup s1
	s4 [e'16 d' c' b] a4 s |
	s4 a' e' c'' |
	b'4 a'2 gis'4 |
	s4 c'' a' e'' |
    % 5
	d''4 c''2 b'4 |
	s4 c'' g' f'' |
	e''4 d''2 cis''4 |
	s4 d'' a' g'' |
	fis''4 e''2 dis''4 |
	% 10
	[e''8 d''] c''2 b'4~b'
	a'2 g'4~g'
	fis'2 e'4~e'
	d'2 c'4~c'
	b2 a4 |
	% 15
	s4 c' dis' fis' |
	a'4 c'' dis'' fis'' |
	a''4 c'''2 [b''8 a''] |
	g''4 f'' e'' d''^\fermata |
	s1
	% 20
	s
	s4 \RHtoL a, b, cis |
	d4 e f2 |
	\RHtoR f4 aes b d' |
	f'4 aes' b' d'' |
	% 25
	f''4 aes''2 [g''8 f''] |
	ees''4 d'' c'' d'' |
	g'4 b' c'' \RHtoL f |
	e4 e'2 \RHtoR d''4 |
	e'4 b' c'' \RHtoL b, |
	% 30
	a,4 a \RHtoR c'' d'' |
	a4 b c' d' |
	e'4 f' g' a' |
	b'4 [a'8 g'] g''4 c'' |
	c''4~[c''16 b' c'' d''] d''4.^\prall c''8 |
	% 35
	c''4 c''' b'' bes''
	a'' aes''2 g''4~g''
	f''2 dis''4~dis''
	d''2 c''4~c''
	b'4 c'' d'' |
	% 40
	e'4 f' g' a' |
	b'4 [a'8 g'] g''4 c'' |
	c''4~[c''16 b' c'' d''] d''4.^\prall c''8 |
	c''4 [g''16 f'' e'' d''] c''4 s |
	s4 [g16 f e d] c4 s |
	s4 c'' g'
	<
	  { f''4 e'' c'' d'' b' c''2. }
	  \context Voice = anotherV { a'4 g'2 f'2 e'2. }
	> s4 |


	s4 [g'16 f' e' d'] c'4 s |
	s4 c'' g' ees'' ~
	< { ees'' d''2 c''4 }
	  \context Voice = anotherV { \stemdown g'2 fis'2 }
	> 
	  |
	bes'4 s2 [d'16 c' bes a] |
	g4 s2 <bes'4 g''>  |
	<a' g''> <d'' f''> <d'' f''>  <cis'' e''> |
	d''4 [a''16 g'' f'' e''] d''4 s |
	s4 [a16 g f e] d4 s |
	s4 d'' a' f'' |
	<
	   { f''4 e'' e'' d'' }
	   \context Voice = anotherV { \stemdown bes'2 bes'2 }
	> |
	< a'4 cis''> s2. |
	\RHtoL s4 a' gis' g' |
	fis'4 f' e'2 |
	dis'4 d' cis' c' |
	b4 bes a2 |
	gis4 g fis f |
	\RHtoR s4 e gis b |
	d'4 f' gis' b' |
	d''4 f''2 e''8 d'' |
	c''4 b' a' g'^\fermata |
	s1 s
	\RHtoL s4 d e fis |
	g4 a bes2 |
	\RHtoR e'4 g' bes' e'' |
	bes''2. [a''8 gis''] |
	a''4 g''2 [f''8 e''] |
	f''4 e''2 [d''8 cis''] |
	d''4 c''2 [b'8 a'] |
	gis'4 gis'' a'' \RHtoL d |
	c4 c'2 \RHtoR b''4 |
	c''4 gis'' a'' \RHtoL g, |
	f,4 f2 \RHtoR b''4 |
	f'4 gis' a' b' |
	c''4 d'' e'' f'' |
	gis'4 [a'8 b'] e'4 d'' |
	< \context Voice = mainV { c''4 d'' s2 }
	  \context Voice = anotherV { \stemdown a'2 <b'2 gis'> } 
	> 
	a'4 a'' gis'' g'' |
	fis''4 f''2 e''4~e''
	d''2 c''4~c''
	b'2 a'4~a'
	gis'4 a' d'' |
	< \context Voice = mainV { c''4 d'' s2 }
	  \context Voice = anotherV { \stemdown <a'2 e'> <gis' b'> }  |
	>
	a'4 s2 [e'16 d' c' b] |
	a4 s2 <f'4 a' d''4> | %% added some chording.
	<e'2 a' c''> <e' gis' b'> |
	a'2.  |
	}>>


     \context Staff = lower <{

	\stemdown s2.  [e''16 d'' c'' b'] |

	a'4
	     s2
     	     \clef bass;	% HWN
	     [e16 d c b,] |
	a,4 s2 a4 |
	e4 c' b2 |
	a4 s2 c'4 |
	g4 e' d'2 |
	c'4 s2 d'4 |
	a4 f' e'2 |
	d'4 s2 e'4 |
	b4 g' fis'2 |
	e'2 dis'4 d' |
	cis'4 c' b2 |
	bes4 a gis g |
	fis4 f e2 |
	dis4 d cis c |
	b,2  s |
	s1
	s4 b, cis dis |
	e4 f g2 |
	\LHtoR g4 bes cis' e' |
	g'4 bes' cis'' e'' |
	g''4 bes''2 [a''8 g''] |
	f''4 e'' d'' c''^\fermata |
	s1 s
	\LHtoL s4 g, a, b, |
	c4 d dis f |
	g4 g'2 \LHtoR d''4 |
	g'4 b' c'' \LHtoL d |
	c4 c'2 \LHtoR d''4 |
	c'4 b'2 \LHtoL g,4 |
	f,4 f e d |
	c4 d e f |
	g,4 g e f |
	g4 f g g, |
	c2 s
	  \LHtoR   %% added. HWN
	s4 c'' b' bes' |
	a'4 gis' g'2 |
	fis'4 f' e' dis'
	  \LHtoL
	  |
	d4 d' c' b |
	c4 d e f |
	g,4 g e f |
	g4 f g g, |
	c2 s4 [g'16 f' e' d'] |
	c'4 s2 [g,16 f, e, d,] |
	c,4 c s f |
	g2 g, |
	c2. \LHtoR [g''16 f'' e'' d''] |

	c''4 \LHtoL
	   s2 [g16 f e d] |
	c4 s2 c'4 |
	d'2 d |
	g4
	  \LHtoR
	[d''16 c'' bes' a'] g'4  \LHtoL s2
	[d16 c bes, a,] g,4 g |
	a2 a, |
	d2 s4 \LHtoR [a'16 g' f' e'] |
	d'4 \LHtoL s2 [a,16 g, f, e,] |
	d,4 s2 d'4 |
	g2 g, |
	a,4 \LHtoR a'' gis'' g'' |
	fis''4 f''2 e''4~e''
	d''2 c''4~c''
	b'2 a'4~a'
	g'2 f'4~f'
	e'2 d'4 |
	\LHtoL e,2^\fermata s |
	s1
	s4 e fis gis |
	a4 b c'2 |
	\LHtoR fis4 a c' ees' |
	fis'4 a' c'' ees'' |
	c'''2.  [bes''8 a''] |
	bes''4 a'' g'' f'' |
	s1
	\LHtoL s4 c d e |
	f2 e |
	d2 c |
	b,2 a, |
	e4 e'2 \LHtoR b''4 |
	e''4 gis'' a'' \LHtoL b, |
	a,4 a2 \LHtoR b''4 |
	a'4 gis'' a'' \LHtoL e, |
	d,4 d' c' b |
	a4 b c' d' |
	e4 d' c' d' |
	e'4 d' e' e |
	a2 s |
	s4 a' gis' g' |
	fis'4 f' e'2 |
	dis'4 d' cis' c' |
	<d'2 b>  <c'4 a> d |
	e4 d e e, |
	a,4 \LHtoR [e''16 d'' c'' b'] a'4 \LHtoL s2
	[e16 d c b,] a,4 d |
	e2 e, |
	a,2.
	}  
     > > 

 

}
\version "1.1.66";
