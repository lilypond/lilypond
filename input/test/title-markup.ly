\version "2.3.17"

%{
   Markup titles also available for direct PostScript output:

   export GS_LIB=$(pwd)/mf/out:/usr/share/texmf/fonts/type1/bluesky/cm
   lilypond-bin -fps input/title/title-markup.ly

  PostScript fonts: WIP.

  * Nonstandardised install directory / how to locate a ps font?
  * Nonstandardised filenames?


For century schoolbook font:

  Debian:
    cp -pv /usr/share/fonts/type1/gsfonts/c*.{afm,pfb} mf/out

  Red Hat (untested):

    cp -pv /usr/share/fonts/afms/adobe/c*.{afm,pfb} mf/out/

    cp -pv /usr/share/fonts/default/Type1/c*.{pfb,afm} mf/out

%}

\paper{
    #(define page-breaking ly:optimal-page-breaks)
    %% Ughr, this breaks TeX output...
    %% fonts = #(make-century-schoolbook-tree 1.0)
    inputencoding = #"latin1"
}

latinTest = \markup {
    "Hellö"
}
    
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

\encoding "latin1"

spaceTest = \markup { "two space chars" }
\header {
    texidoc = "Make titles using markup.  Only in direct PostScript output."

    tagline = "my tagline for "
    
    %dedication = "För my dør Lily"
    % ugh: encoding char-size
    %dedication = "For my öòóôõø so dear Lily"
    dedication = \markup { "For my "
			   "öòóôõø"
			   " so dear Lily" }
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
    piece = "Piece"

    %% Override automatic book title
    %% bookTitle = \markup { \fill-line < \huge\bold \title > > }
}

\book {
    
    \score {
	\context Staff  \relative c' {
	    c2-\sizeTest c2-\spaceTest
	}
	\paper {
	    #(paper-set-staff-size (* 11.0 pt)) 
	}
    }
    
    \score {
	\context Staff  \relative c' {
	    %% stress page breaking:
	    %% 35 keep on 3 pages
	    %% 36 spread evenly over 4 pages
	    \repeat unfold 6 { a b c d \break }
	    
	    %% FIXME: TODO factor \pagebreak \noPagebreak into regtest
	    %% Without this, page breaks are better, after measure: 12
	    \noPageBreak
	    \repeat unfold 30 { a b c d \break }
	    c1
	}
	\header {
	    %% Override automatic score title
	    %% scoreTitle = \markup { "Tweetje" }
	    opus = "opus 1"
	    piece = "Second"
	}
	\paper {
	}
    }
}
