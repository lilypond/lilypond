\version "1.1.52";

\score {
       \notes{ 
       
         \clef "treble"; c'1^"{treble}" \bar "||";
         \clef "french";c'1^"{french}" \bar "||";
         \clef "soprano";c'1^"{soprano}" \bar "||";
         \clef "mezzosoprano";c'1^"{mezzosoprano}" \bar "||";
         \clef "alto";c'1^"{alto}" \bar "||";
         \clef "tenor";c'1^"{tenor}" \bar "||";
         \clef "baritone";c'1^"{baritone}" \bar "||";
         \clef "varbaritone";c'1^"{varbaritone}" \bar "||";
         \clef "G_8";c'1^"{sub 8?}" \bar "||";
         \clef "G^8";c'1^"{sup 8?}" \bar "||";
         \clef "bass";c'1^"{bass}" \bar "||";
         \clef "subbass";c'1^"{subbass}" \bar "||";
	\property Staff.clefStyle="transparent"
         \clef "treble"; c'1^"clefStyle=\"transparent\"" \bar "||";
	\property Staff.clefStyle="fullSizeChanges"
         \clef "treble"; c'1^"clefStyle=\"fullSizeChanges\"" \bar "|.";
         }
         \paper{
         }
}

