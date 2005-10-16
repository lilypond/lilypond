\version "2.7.13"
\header {
	texidoc = "If two forced accidentals happen at the same time, only one
	sharp sign is printed."
}

\layout { raggedright= ##t }

\transpose c c'
\context Staff <<
  \key g \major
  \context Voice = "va" { \stemUp c' fis! }
  \context Voice = "vb" { \stemDown c fis! }
>>





