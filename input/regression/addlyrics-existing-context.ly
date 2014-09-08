\version "2.19.14"

\header {
  texidoc = "
@code{\\addlyrics} should be able to attach itself to named and unnamed @code{Voice}
constructs.  For all tests where this succeeds, the noteheads will be red."
}

\layout { ragged-right = ##t }

\new Staff \new Voice \with { \override NoteHead.color = #red }
{ \tempo \markup \typewriter "\\new Staff \\new Voice" c'1 }
\addlyrics { Oh! }

\new Voice \with { \override NoteHead.color = #red }
{ \tempo \markup \typewriter "\\new Voice" c'1 }
\addlyrics { Oh! }

\new Staff \new Voice = "named" \with { \override NoteHead.color = #red }
{ \tempo \markup \typewriter "\\new Staff \\new Voice = \"named\"" c'1 }
\addlyrics { Oh! }

\new Voice = "named" \with { \override NoteHead.color = #red }
{ \tempo \markup \typewriter "\\new Voice = \"named\"" c'1 }
\addlyrics { Oh! }
