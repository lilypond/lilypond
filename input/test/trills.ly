\version "2.3.4"

\header {
  texidoc="@cindex Trills
Trills, pralls and turns may also be written out in full. Here the
D'Anglebert system (1689) is shown."
  
}

%{

taken from :

title="Marques des agr\\'ements et leur signification"
  composer="D'Anglebert"
  year="1689"

%}

invisible = \override NoteHead  #'transparent = ##t
visible = \revert NoteHead #'transparent



\score {
  <<
    \context GrandStaff <<
      \new Staff \relative c'' {
	\time 1/4
	c4\prall
	\time 3/8
	c4.^\lineprall
	c4.\downprall
	c4.\upprall
	\time 5/8
	c4\turn c4.\upprall
	d4\turn b4.\downprall
	\time 3/4
	b4 c^\turn d 
	\time 1/4
	<<{ d4}\\
	{ 
	   \override Script  #'extra-offset = #'(-0.8 . 2.0)
	   b_\turn}>>
%{ FIXME  \comma does not exist
	\startHorizScript
 
	c^\comma
	\time 3/8
	c4.^\comma
	b8 c4_\comma 
	c8 b4_\comma
	b8 c4^\comma _\comma
	\endHorizScript
%}
        \time  2/2 \slurDown
	<<{c2}{s4 \invisible d1*1/4 ( \visible }>>  e2)
	<<{a,2}{s4 \invisible b1*1/4 ( \visible }>>  d2)
	<<{a2}{s4 \invisible b1*1/4 ( \visible }>>  d2)
	\time 3/4
	d2^\prallup e4
      }
      \new Lyrics \lyrics {
	"Tremblement"4
	"Tremblement"4.
	"Cadence"
	"autre"
	"Double"4 "cadence"4.
%{	"autre"4 _4.
	_4 "sans tremblement"2
	"sur une tierce"4
	"Pinc\\'e"4
	"autre"4.
	"Cheute ou Port de"
	"en descendant"
	"Cheute et pinc\\'e"
	"Coul\\'e Sur 2 notes"1
	"autre"
	"autre"
	"Double cadence"2.
%}

    }
      \new Lyrics \lyrics {
	"simple"4
	"appuy\\'e"4.
%{	\skip 1*3
	\skip 4 \skip 4.
	"voix et montant"
	\skip 2.
	"de suite"1
%}
      }
      \new Staff \relative c'' {
        % autobeamer has som problems here
	d32[  \repeat unfold 3 { c d } c]
	d8 ~  d32[\repeat unfold 3 { c32 d  } c]
	d32 c b c \repeat unfold 4 { d32 c }
	b32 c d c \repeat unfold 4 { d32 c }
	c32[( b a16 b  c)]
	b32[ \repeat unfold 5 { c d32 } c]
      }
    >>
  >>
	\paper{ }
}

