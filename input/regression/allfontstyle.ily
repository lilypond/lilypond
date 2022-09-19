\version "2.23.14"

\header{
    texidoc="
Different text styles are used for various purposes.
"
}

\paper {
    ragged-right = ##t
}

\relative c'' \context Staff {
    \textLengthOff
    \repeat volta 2 { \time 4/4 c4^"cuivre"_\fermata }
    \alternative {
	{
	    d-4_\markup { \italic "cantabile"  } }
	{  e }  } \acciaccatura { c16 }

    f4\ff^\markup  { \large "Largo" } \textMark "B" g
}
