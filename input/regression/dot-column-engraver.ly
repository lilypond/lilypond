\version "2.19.21"

\header {
  texidoc = "Dot Columns are engraved in the Staff by default,
enabling dots to move vertically to make room for dots from another voice.
If Dot_column_engraver is moved to Voice, separate dot columns are engraved,
and these dots avoid notes in other voices."
}

music = \relative { \time 3/4 << {
  <d'' f g>4. c c b g f a <a b> a <a' b>
} \\ \tuplet 2/1 {
  <f, g b>2. a-- <a b> <g a>-. a2. a-- a a b <a b> \bar "|."
} >> }

\score{ \music }
\markup "move Dot_column_engraver to Voice :"
\score{ \music
\layout {
  \context {\Staff \remove "Dot_column_engraver"}
  \context {\Voice \consists "Dot_column_engraver"}
}}
