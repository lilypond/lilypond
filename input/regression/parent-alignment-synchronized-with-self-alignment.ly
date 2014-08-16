\version "2.19.11"

\header {
  texidoc = "When @code{parent-alignment-X} property is unset,
the value of @code{self-alignment-X} will be used as the factor
for parent alignment.  This happens e.g. for LyricTexts."
}

{
  \time 4/2
  % use breve noteheads because their refpoints aren't on their
  % left edges - this may help catching subtle bugs.
  \override NoteHead.style = #'altdefault
  <>^"alignments “synchronized”:"
  f'\breve f' f'
  <>^"parent-alignment set to ##f:"
  f' f' f'
}
\addlyrics {
  \override LyricSpace.minimum-distance = 5

  \override LyricText.self-alignment-X = #LEFT
  left
  \override LyricText.self-alignment-X = #CENTER
  center
  \override LyricText.self-alignment-X = #RIGHT
  right

  \override LyricText.parent-alignment-X = ##f
  \override LyricText.self-alignment-X = #LEFT
  left
  \override LyricText.self-alignment-X = #CENTER
  center
  \override LyricText.self-alignment-X = #RIGHT
  right
}
