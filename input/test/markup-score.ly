
\header {
    %% WIP
    texidoc = "Use \\score block as markup command."
    
}

\version "2.3.1"


\paper {
    raggedright = ##t
    vsize = 30 \mm
    linewidth = 30\mm
    indent = 0 \mm
    hsize = 40\mm
}
\header {
    title = "title"
    subtitle = \markup { \fill-line <
	"subtitle with score: "
	\score { \relative \notes { a'^"Hi" b c } }
	"woo!"
    > }
    subsubtitle = "subsubtitle"
}

\paper {
    raggedright = ##f
    linewidth = 150\mm
    indent = 15\mm
    vsize = 298\mm
    hsize = 210 \mm
}

\relative {
    a' b c d \break
    a b c d \break
    a b c d \break
}

