#(ly:set-option 'old-relative)
\version "1.9.2"
\header{
texidoc="
Breathing signs, also used for phrasing, do normally not influence
global spacing -- only if space gets tight, notes are shifted to make
room for the breathing sign. Breathing signs break beams running
through their voice. In the following example, the notes in the first
two measures all have the same distance from each other.

Breathing signs are available in different tastes: commas (default),
ticks, vees and `railroad tracks' (caesura).

Gregorian chant notation sometimes also uses commas and ticks, but in
smaller font size (we call it 'virgula' and 'caesura').  However, the
most common breathing signs are divisio minima/maior/maxima and
finalis, the latter three looking similar to bar glyphs.

" }

\include "gregorian-init.ly"

\score {

  {
    %
    % Modern notation:
    %
    \new Staff {
      \notes \relative c'' {
	\key es \major \time 3/4

	% this bar contains no \breathe
	<
	  { g4 as g } \\
	  { es4 bes es }
	> |

	% by default, \breathe uses the rcomma, just as if saying:
	% \property Voice.BreathingSign \set #'text =
	%	#(make-musicglyph-markup "scripts-rcomma")
	<
	  { g4 as g } \\
	  { es4 \breathe bes es }
	> |

	% rvarcomma and lvarcomma are variations of the default rcomma
	% and lcomma

    	% N.B.: must use Staff context here, since we start a Voice below
	\property Staff.BreathingSign \set #'text =
		#(make-musicglyph-markup "scripts-rvarcomma")
	<
	  { g4 as g } \\
	  { es4 \breathe bes es }
	> |

	% wedge
	\property Voice.BreathingSign \set #'text =
		 #(make-musicglyph-markup "scripts-upbow")
	es8 d es f g8 \breathe f |

	% caesura
	\property Voice.BreathingSign \set #'text =
		 #(make-musicglyph-markup "scripts-caesura")
	es8[ d] \breathe  es[ f g f] |
	es2 r4 \bar "||" \break
      }
    }

    %
    % Gregorian notation:
    %
    \context VaticanaStaff {
      \notes \relative c' {

	% we turn bars off for Gregorian stuff
	\property Staff.BarLine \override #'transparent = ##t

	% here is no \breathe
	c g c

	% \virgula applies rcomma, but in a smaller font
	c \virgula g c

	% \caesura applies rvarcomma, but in a smaller font
	c \caesura g c

	% \divisioMinima is a simple vertical stroke through the
	% uppermost staffline, just like the original implementation
	% of breathing signs.
	c \divisioMinima g c

	% \divisio{maior,maxima} and \finalis look like bars and are
	% vertically centered on the staff; the direction property has
	% no effect
	c \divisioMaior g c
	c \divisioMaxima g c

	% this one looks almost like a "||" type bar
	\finalis
      }
    }
  }
}
