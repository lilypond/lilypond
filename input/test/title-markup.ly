\version "2.1.29"
%{
   \markup in titles is WIP.

   only available in direct PostScript output:

   export GS_LIB=$(pwd)/mf/out:/usr/share/texmf/fonts/type1/bluesky/cm
   lilypond-bin -fps input/title/title-markup.ly

%}

latinTest = \markup { \latin-i "Hellö" }
    
sizeTest = \markup {
	\column <
            { \normalsize "normalsize"
              \hspace #5
              \smaller "smaller"
              \hspace #5
              \smaller \smaller "smaller"
              \hspace #5
              \smaller \smaller \smaller "smaller"
            }
            " " 
            { \normalsize "normalsize"
              \hspace #5
              \bigger "bigger"
              \hspace #5
              \bigger \bigger "bigger"
              \hspace #5
              \bigger \bigger \bigger "bigger"
            }
       >
}

spaceTest = \markup { "two space chars" }
\header {
    texidoc = "Make titles using markup (WIP)."

    tagline = "my tagline for v \version"
    copyright = "copyright by me"
    
    %dedication = "För my dør Lily"
    % ugh: encoding char-size
    dedication = "For my öòóôõø so dear Lily"
    title = "Title"
    subtitle = "(and (the) subtitle)"
    subsubtitle = "Sub sub title"
    poet = "Poet"
    composer = "Composer"
    texttranslator = "Text Translator"
    opus = "opus 0"
    meter = "Meter (huh?)"
    arranger = "Arranger"
    instrument = "Instrument"
    piece = "piece"

    %% this overrides automatic book title
    xxbookTitle = \markup {
	\column <
	    %\fill-line #linewidth < \huge \bigger \bold \title >
            \override #'(baseline-skip . 4) \column <
	        \fill-line < \latin-i \dedication >
	        \fill-line < \huge\bigger\bigger\bigger\bigger \bold \title >
                \override #'(baseline-skip . 3) \column <
                    \fill-line < \large\bigger\bigger \bold \subtitle >
                    \fill-line < \bigger\bigger \bold \subsubtitle >
                >
                \override #'(baseline-skip . 5) \column <
                " "
                >
                \override #'(baseline-skip . 2.5) \column <
	            \fill-line < \bigger \poet
                                 \large\bigger \caps \composer >
		    \fill-line < \bigger \texttranslator
				 \bigger \opus >
		    \fill-line < \bigger \meter
				 \bigger \arranger >
                    " "
		    \fill-line < \large\bigger \instrument >
                    " "
		    \fill-line < \large\bigger \caps \piece  " ">
                >
            >
        >    
    }

%{
     foe = \sizeTest
     baar = \spaceTest
%}
}

\score {
    \context Staff \notes \relative c' {
	c2-\sizeTest c2-\spaceTest
    }
}

\header {
    %% override automatic score title
    xxscoreTitle = \markup { "Tweetje" }
    opus = "opus 1"
    piece = "Second"
}

\score {
    \context Staff \notes \relative c' {
	\repeat unfold 1 { a b c d \break }
	c1
    }
}
