
\header {
    texidoc = "Lyrics are not lowered despite the presence of an octavation 8."
}

\version "2.3.4"
\paper {
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
<<
\relative c' { \clef "G_8" c c c c }
\newlyrics { bla bla bla bla }
>>
