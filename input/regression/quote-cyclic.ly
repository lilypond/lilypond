
\header {

    texidoc = "Two quoted voices may refer to each other. In this
example, there are notes with each full-bar rest."
	  
 
}
\version "2.3.14"
A = \relative c' { c4 d e f | << R1            \\
				 \quote "qB" 1 >> | }
B = \relative c' { << R1            \\
		      \quote "qA" 1 >> | f4 e d c | }

\addquote "qA" \A
\addquote "qB" \B

\paper { raggedright = ##t }

<<
    \context Staff = "A" \A
    \context Staff = "B" \B
>>
