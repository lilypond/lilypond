\version "2.10.12"

\header { texidoc = "
This snippet shows a way to insert available Feta font symbols as caesurae and ways to create tramlines.  Normally a caesura is indicated by a pair of oblique lines lying through the top line of the staff, also called a fetura, tramlines, railroad tracks, or a cut-off.  LilyPond provides several alternatives.  A stylized version of tramlines is called caesura.  The normal tramline form, here called fetura, is not in the Feta font set, but can be constructed using the / character.
" }

\version "2.8.4"
%{ This snippet shows a way to insert available Feta font symbols as caesurae
 and ways to create tramlines.  Normally a caesura is indicated by a pair of
 oblique lines lying through the top line of the staff, also called a fetura,
 tramlines, railroad tracks, or a cut-off.  LilyPond provides several
 alternatives.  A stylized version of tramlines is called caesura.  The normal
 tramline form, here called fetura, is not in the Feta font set, but can be
 constructed using the / character.%}
						\relative c'' {
\time 3/4
e4 d e
					% default
e_\markup { "default - breathe" }
	\breathe d e |
					% rcomma
	% by default, \breathe uses the rcomma, just as if saying:
	 	\override BreathingSign   #'text =
		#(make-musicglyph-markup "scripts.rcomma")
e_\markup rcomma \breathe d e |
					% lcomma
	 	\override BreathingSign   #'text =
		#(make-musicglyph-markup "scripts.lcomma")
e_\markup lcomma \breathe d e |
					% rvarcomma
		\override BreathingSign   #'text =
		#(make-musicglyph-markup "scripts.rvarcomma")
e_\markup rvarcomma \breathe d e | \break
					% lvarcomma
		\override BreathingSign   #'text =
		#(make-musicglyph-markup "scripts.lvarcomma")
e_\markup lvarcomma \breathe d e |
					% upbow or wedge
		\override BreathingSign   #'text =
		 #(make-musicglyph-markup "scripts.upbow")
e_\markup upbow \breathe d e |
					% caesura
		\override BreathingSign   #'text =
		 #(make-musicglyph-markup "scripts.caesura")
e_\markup caesura \breathe d e |
					% fetura, tramlines, or railroad tracks
<<{
\hideNotes a16 a a a a4 a \unHideNotes
}\\{
	\once\override TextScript   #'extra-offset = #'(4.0 . -2.7)
e^\markup {\fontsize #2 {\bold "/" \hspace #-1.4 \bold "/"}}
d_\markup { \hspace #-10 "fetura" } e |
}>>
	\once\override TextScript   #'font-name = #"cmb10"
	\once\override TextScript   #'extra-offset = #'(4.0 . -2.2)
e^\markup {\magnify #1.3 { "/" \hspace #-1.5 "/"}}
	\once \override Score.SeparationItem #'padding = #4
d_\markup { \hspace #-10 "fetura" } e |

e d e \bar "||"
						}%end relative
					\layout{
indent = 0.00\in
					}%end layout


