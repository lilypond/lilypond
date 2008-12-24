\header {
    texidoc = "Lyrics are not lowered despite the presence of an octavation 8."
}

\version "2.12.0"
\layout {
    \context {
	\Staff
	\override VerticalAxisGroup #'minimum-Y-extent = ##f
    }
    \context {
	\Lyrics
	\override VerticalAxisGroup #'minimum-Y-extent = ##f
    }
    ragged-right = ##t
}

\relative { \clef "G_8" c c c c }
\addlyrics { bla bla bla bla }
