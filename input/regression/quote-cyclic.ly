
\header {

    texidoc = "Two quoted voices may refer to each other. In this
example, there are notes with each full-bar rest."
	  
 
}
\version "2.4.0"
A = \relative c' { c4 d e f | << R1            \\
				 \quote "qB" 1 >> | }
B = \relative c' { << R1            \\
		      \quote "qA" 1 >> | f4 e d c | }

\addquote "qA" \A
\addquote "qB" \B

\layout { raggedright = ##t }

<<
    \context Staff = "A" \A
    \context Staff = "B" \B
>>
