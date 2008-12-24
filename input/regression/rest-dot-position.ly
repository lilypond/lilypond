\header {

    texidoc = "Dots of rests should follow the rest positions. "

}

\version "2.12.0"

\paper { ragged-right = ##t } 

{
    \set Score.timing = ##f
    r\longa. r\breve.
    r1. r2. r4. r8. r16. r32. r64. r64. 
    \bar "" 
    << {
	r\longa. r\breve.
	r1. r2. r4. r8. r16. r32. r64. r64. 
    } \\ {
	r\longa. r\breve.
	r1. r2. r4. r8. r16. r32. r64. r64. 
    } >>
    
}
