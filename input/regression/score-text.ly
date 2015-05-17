\version "2.19.21"

\header {

    texidoc = "Markup texts are rendered above or below a score."

}

\paper {
    line-width = #110
}

%% using Book and Score to get text in lilypond-book
\book {
    \markup {
	\fill-line { "High up above" }
    }
    \score {
	<<
	    \relative {
		\clef bass
		d2 d c4 bes a2 \break
		c2 c d4 f g2
	    }
	    \addlyrics {
		My first Li -- ly song,
		Not much can go wrong!
	    }
	>>
    }
    \markup {
	%%TODO: make \verse markup.
	\fill-line {
	    \line {
		"2. "
		\column {
		    \line { My next Li-ly verse }
		    \line { Now it's getting worse! }
		}
	    }
	}
    }
    \markup {
	\fill-line {
	    \line {
		"3. "
		\column {
		    \line { My last Li-ly text }
		    \line { See what will be next! }
		}
	    }
	}
    }
}
