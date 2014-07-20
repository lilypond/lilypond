\version "2.19.11"

\header {
  texidoc = "Grobs using @code{ly:self-alignment-interface::aligned-on-x-parent}
and @code{ly:self-alignment-interface::aligned-on-y-parent}
callbacks support separate alignments for self and parent."
}

{ f'1 f' f' }
\addlyrics {
  \override LyricSpace.minimum-distance = 5
  \override LyricText.self-alignment-X = #LEFT
  \override LyricText.parent-alignment-X = #LEFT
  left-left
  \override LyricText.self-alignment-X = #LEFT
  \override LyricText.parent-alignment-X = #CENTER
  left-center
  \override LyricText.self-alignment-X = #LEFT
  \override LyricText.parent-alignment-X = #RIGHT
  left-right
}

{ f'1 f' f' }
\addlyrics {
  \override LyricSpace.minimum-distance = 5
  \override LyricText.self-alignment-X = #CENTER
  \override LyricText.parent-alignment-X = #LEFT
  center-left
  \override LyricText.self-alignment-X = #CENTER
  \override LyricText.parent-alignment-X = #CENTER
  center-center
  \override LyricText.self-alignment-X = #CENTER
  \override LyricText.parent-alignment-X = #RIGHT
  center-right
}

{ f'1 f' f' }
\addlyrics {
  \override LyricSpace.minimum-distance = 5
  \override LyricText.self-alignment-X = #RIGHT
  \override LyricText.parent-alignment-X = #LEFT
  right-left
  \override LyricText.self-alignment-X = #RIGHT
  \override LyricText.parent-alignment-X = #CENTER
  right-center
  \override LyricText.self-alignment-X = #RIGHT
  \override LyricText.parent-alignment-X = #RIGHT
  right-right
}
