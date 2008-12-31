#(ly:set-option 'old-relative)
\version "2.12.0"

%{
 Header for Petites Preludes.

 Six Petits Preludes,
 Collection Johann Peter Kellner
 ca 1703 - 1707

 Kellner was a student of Bach's that copied some 90 works of his master.
%}




\header{
  title =	 "Praeludium"
  composer =	 "Johann Sebastian Bach (1685-1750)"
  enteredby =	 "JCN,HWN"
  opus= "BWV 940"


  % mutopia headers.
  mutopiacomposer = "J.S.Bach (1685-1750)"
  mutopiaopus = "BWV940"

  mutopiainstrument = "Harpsichord,Clavichord"
  date = "1700s"
  source = "Ed. Henry Lemoine Urtext"
  style = "Baroque"
  copyright = "Public Domain"
  maintainer = "Jan Nieuwenhuizen"
  maintainerEmail = "janneke@gnu.org"
  lastupdated =	 "2003/Aug/22"
  mutopiapublicdomain = "\\parbox{\paper-width}{\\thefooter\\quad\\small
    \\\\This music is part of the Mutopia project,
    \\texttt{http://www.mutopiaproject.org/}\\\\It has been typeset
    and placed in the public domain by " + \maintainer +
    ".\\\\Unrestricted modification and redistribution is permitted
    and encouraged---copy this music and share it.}"
 tagline = \mutopiapublicdomain
 footer = "Mutopia-2003/08/22-nr"
}

one = \relative c{
	\skip 1 |
	\stemUp
	r4 d''2 cis4 |
	\stemNeutral
	d16 a' g f  e f cis d 
	e8.\mordent f16  d8.\prall cis16 |
	\stemUp\tieUp
	cis4 ~ cis16 a d8 ~ d4 a |
	%5
	b2 ~ b4 a ~ |
	a16 a \stemUp g! f g4 ~ g f ~ |
	f16 a g f  e16 g8. ~  g16 g f e  d f8. ~ |
	f16 f e d b'4 a g |

	fis4 g r8 g16 bes e4 |
	%10
	d1
	\bar "|."
}

two = \relative c{
	r16 d'' c bes  a bes f g 
	a8.\mordent bes16  g8.\prall f16 |
	\stemDown
	f2 e2 |
	\change Staff=lower \stemUp
	r4 a, bes b |
	\change Staff=upper \stemDown\tieDown
	r16 b' a g  f8. f16 e2 ~ |
	%5
	e2 ~ e4 ~ e16 e f! d |
	s4 e4_\mordent~ e4 d4 ~ |
	d4. cis16 d cis4 d ~ |
	d8 r r16 e f d r16 e f d r d e cis |
	r16 e d c!  bes! d8. s4 r16 bes' a g |
	%10
	fis1
}

three = \relative c{
	\stemUp
	f2 e |
	\stemNeutral
	d16 d' c bes  a bes f g 
	a8.\mordent bes16 
	g8.\prall f16 |
	f2 g4 gis |
	a2 ~ a16 a g f  e f c d |
	%5
	e8.\mordent f16  d8.\prall c16 \stemNeutral c4. d8 |
	\stemDown	\tieDown

	e4 ~ e16 f e d  cis a b cis  d e f d |
	\override TextScript  #'font-style = #'finger
	bes2 a ~ |
	a a |
	d, cis' |
	%10
	a'1
	\bar "|."
}

four = \relative c{
	\stemDown 
	d2 cis |
	\skip 1*2 |
	\skip 4*3
	\change Staff=upper \stemUp
	\override NoteColumn #'horizontal-shift = #1
	c''4 |
	%5
	a gis ~ gis16 gis fis e 
	\skip 4*1
	\change Staff=lower \stemDown
	\override NoteColumn   #'horizontal-shift = #0
	\stemUp\tieUp
	b2 a |
	g a4. gis16 a |
	gis2 < g cis,>8 <f d> e4 |
	d4. fis16 g r16 bes8. ~ bes4 |
	%10
	\stemDown
	<< d,1  { \textLengthOn s4^\markup { \hspace #20 }
		 s4^\markup { \hspace #1 }  s4 }
	     >>
}

global = {
	\time 4/4
	\key f \major
}

\score{
	% Allegretto
	\new PianoStaff <<
		\new Staff = "upper" <<
			\global
			\new Voice = "i"\one
			\new Voice = "ii" \two
		>>
		\new Staff = "lower" <<
			\global
			\clef "bass"
			\new Voice = "iii" \three
			\new Voice = "iv" \four
		>>
	>>
	\layout{
		line-width = 17.0 \cm  
		\context {
		    \Score
		    \override SpacingSpanner #'spacing-increment = #2.0
		}
	}
	
  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 40 4)
      }
    }


}


