\version "2.11.51"
\header{
  texidoc="
Breathing signs are available in different tastes: commas (default),
ticks, vees and `railroad tracks' (caesura)."
}


{
  %% Modern notation:
  \new Staff {
    \relative c'' {
      \key es \major \time 3/4

      %% this bar contains no \breathe
      <<
	{ g4 as g } \\
	{ es4 bes es }
      >> |

      %% by default, \breathe uses the rcomma, just as if saying:
      %% \override BreathingSign  #'text =
				%	#(make-musicglyph-markup "scripts.rcomma")
      <<
	{ g4 as g } \\
	{ es4 \breathe bes es }
      >> |

      %% rvarcomma and lvarcomma are variations of the default rcomma
      %% and lcomma

      %% N.B.: must use Staff context here, since we start a Voice below
      \override Staff.BreathingSign  #'text =
      #(make-musicglyph-markup "scripts.rvarcomma")
      <<
	{ g4 as g } \\
	{ es4 \breathe bes es }
      >> |

      %% wedge
      \override BreathingSign  #'text =
      #(make-musicglyph-markup "scripts.upbow")
      es8 d es f g8 \breathe f |

      %% caesurae
      \override BreathingSign  #'text =
      #(make-musicglyph-markup "scripts.caesura.curved.curved")
      es8[ d] \breathe
      \override BreathingSign  #'text =
      #(make-musicglyph-markup "scripts.caesura.curved.straight")
                       es[ f] \breathe g[ f] |
      es2 r4 \bar "||" \break
    }
  }
}
