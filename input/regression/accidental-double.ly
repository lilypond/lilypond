\version "2.7.32"
\header {
	texidoc = "If two forced accidentals happen at the same time, only one
	sharp sign is printed."
}

\layout { ragged-right= ##t }

\transpose c c'
\context Staff <<
  \key g \major
  \context Voice = "va" { \stemUp c' fis! }
  \context Voice = "vb" { \stemDown c fis! }
>>





