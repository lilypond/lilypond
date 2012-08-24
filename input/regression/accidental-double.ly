\version "2.16.0"
\header {
	texidoc = "If two forced accidentals happen at the same time, only one
	sharp sign is printed."
}

\layout { ragged-right= ##t }

\transpose c c'
\context Staff <<
  \key g \major
  \new Voice { \stemUp c' fis! }
  \new Voice { \stemDown c fis! }
>>





