
\header {
    texidoc = "Ambituses indicate pitch ranges for voices.

By default, the ambitus grob is put before the clef.  You can control
this behaviour through the @code{breakAlignOrder} property of the score
context by redefining the order.


The shape of the note heads to use can be changed via the
@code{note-head-style} property, which holds the glyph name of the
note head.  The vertical line between the upper and lower head can be
switched on or off via the @code{join-heads} property.

"
}

%{

 tex chokes on #

, e.g. with the following addition to the
paper block:

@example
\context @{
  \Score
\override BreakAlignment #'break-align-orders = #(make-vector 3 '(
    instrument-name
    left-edge
    span-bar
    breathing-sign
    clef
    ambitus
    key-signature
    staff-bar
    time-signature
    custos
  ))
@}
@end example

 
@example
\context @{
  \Voice
  \consists Ambitus_engraver
  Ambitus \set #'note-head-style = #'noteheads-2mensural
  Ambitus \set #'join-heads = ##f
@}
@end example


 %}
\version "2.3.2"

upper = \notes \relative c {
	\clef "treble"
	\key c \minor
	as'' c e bes f cis d e f g f e d f d e
	f d e e d f d e e d f d e e d f d e
	f d e e d f d e e d f d e e d f d e
}

lower = \notes \relative c {
	\clef "treble"
	\key e \major
	e'2 b4 g a c es fis a cis b a g f e d
	f e d e f g f e d e f g f e d e f g
	f e d e f g f e d e f g f e d e f g
}

\score { \context ChoirStaff {
	<<
		\new Staff { \upper }
		\new Staff { \lower }
	>> }
	\paper {
	       \context {
			\Score
\override BreakAlignment #'break-align-orders = #(make-vector 3 '(
				instrument-name
				left-edge
				ambitus
				span-bar
				breathing-sign
				clef
				key-signature
				staff-bar
				time-signature
				custos
			))
		}
		\context {
			\Voice
			\consists Ambitus_engraver
		}
	}
}

