\version "2.1.29"
%{
   \markup in titles is WIP.

   only available when compiled with PAGE_LAYOUT is #define'd
   see include/paper-book.hh

%}

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
    
    %dedication = "För my dør Lily"
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

%{
    makeConditionalTitle = \markup {
	\column <
            #(if (defined? 'title)
             (markup* #:fill-line (#:huge #:bigger #:bigger #:bold title))
             (markup* ""))
            #(if (defined? 'subtitle)
             (markup* #:fill-line ( #:large #:bold subtitle))
             (markup* ""))
            #(if (defined? 'subsubtitle)
             (markup* #:fill-line (subsubtitle))
             (markup* ""))
	 >
    }
%}
    bookTitle = \markup {
	\column <
	    %\fill-line #linewidth < \huge \bigger \bold \title >
            \override #'(baseline-skip . 4) \column <
	        \fill-line < \dedication >
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
	% \break c2 c2
    }
}

\header {
    scoreTitle = \markup { "Tweetje" }
}

\score {
    \context Staff \notes \relative c' {
	c2-\sizeTest c2-\spaceTest
	% \break c2 c2
    }
}
