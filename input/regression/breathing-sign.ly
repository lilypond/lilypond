#(ly:set-option 'old-relative)
\version "1.9.8"
\header{
texidoc="
Breathing signs are available in different tastes: commas (default),
ticks, vees and `railroad tracks' (caesura)."
 }


\score {

  {
    %
    % Modern notation:
    %
    \new Staff {
      \notes \relative c'' {
	\key es \major \time 3/4

	% this bar contains no \breathe
	<<
	  { g4 as g } \\
	  { es4 bes es }
	>> |

	% by default, \breathe uses the rcomma, just as if saying:
	% \property Voice.BreathingSign \set #'text =
	%	#(make-musicglyph-markup "scripts-rcomma")
	<<
	  { g4 as g } \\
	  { es4 \breathe bes es }
	>> |

	% rvarcomma and lvarcomma are variations of the default rcomma
	% and lcomma

    	% N.B.: must use Staff context here, since we start a Voice below
	\property Staff.BreathingSign \set #'text =
		#(make-musicglyph-markup "scripts-rvarcomma")
	<<
	  { g4 as g } \\
	  { es4 \breathe bes es }
	>> |

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
  }
}
