\version "2.4.0"
\header {
	texidoc = "If two forced accidentals happen at the same time, only one
	sharp sign is printed."
}


\score {  \transpose c c'
   \context Staff <<
     \key g \major
     \context Voice = "va" { \stemUp c' fis! }
     \context Voice = "vb" { \stemDown c fis! }
   >>

    \layout { raggedright= ##t }
	 
}



