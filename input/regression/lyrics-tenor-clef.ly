\header {
    texidoc = "Lyrics are not lowered despite the presence of an octavation 8."
}

\version "2.3.22"
\layout {
    \context {
	\Staff
	minimumVerticalExtent = ##f
    }
    \context {
	\Lyrics
	minimumVerticalExtent = ##f
    }
    raggedright = ##t
}

\relative { \clef "G_8" c c c c }
\addlyrics { bla bla bla bla }
