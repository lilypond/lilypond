\version "1.3.146"

\header{
texidoc="
The transparent clef should not occupy any space and with style
@code{fullSizeChanges}, the changing clef should be typeset in full
size. For octaviated clefs, the ``8'' should appear closely above or
below the clef respectively.  The ``8'' is processed in a convoluted
way, so this is fragile as well.
"
}



\score {
       \notes{ 
       
         \clef "treble" c'1^"{treble}" \bar "||"
         \clef "french"c'1^"{french}" \bar "||"
         \clef "soprano"c'1^"{soprano}" \bar "||"
         \clef "mezzosoprano"c'1^"{mezzosoprano}" \bar "||"
         \clef "alto"c'1^"{alto}" \bar "||"
         \clef "tenor"c'1^"{tenor}" \bar "||"
         \clef "baritone"c'1^"{baritone}" \bar "||"
         \clef "varbaritone"c'1^"{varbaritone}" \bar "||"
         \clef "G_8"c'1^"{sub 8?}" c'1 \bar "||"
         \clef "G^8"c'1^"{sup 8?}" c'1 \bar "||"
         \clef "bass"c'1^"{bass}" \bar "||"
         \clef "subbass"c'1^"{subbass}" \bar "||"
	 \property Staff.Clef \override #'transparent = ##t
         \clef "treble" c'1^"transparent=\#t" \bar "||"
	 \property Staff.Clef \override #'transparent = ##f
	 \context Staff \outputproperty #(make-type-checker 'clef-interface) #'full-size-change = ##t
         \clef "french" c'1^"full-size-change = \#t" \bar "|."
         }
         \paper{
	   \translator{
	     \StaffContext
%	     Clef \override #'full-size-change = ##t
	   }
         }
}

