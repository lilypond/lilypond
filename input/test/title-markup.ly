\version "2.1.28"
%{
   \markup in titles is WIP, only available in direct PostScript output
   process and view this file doing:

     lilypond-bin -fps title-markup.ly
     export GS_LIB=$(pwd)/mf/out:/usr/share/texmf/fonts/type1/bluesky/cm
     gs title-markup.ps


FIXME: use conditionals in  \makeTitle:

#(define (my-ly-version)
    (list-head (ly:version) 3))

#(if (not (defined? 'pieceTagLine))
    (define pieceTagLine (string-append "Jeremie " (numbers->string (my-ly-version)) " was here")))

\header{
tagline = \pieceTagLine
texidoc = "

%}

\header {
    texidoc = "Make titles using markup (WIP)."


    % FIXME
    fonts = #'((font-family . roman)
 		  (word-space . 1)
 		  (baseline-skip . 2)
 		  (font-series . medium)
 		  (font-style . roman)
 		  (font-shape . upright)
 		  (font-size . 0))
    
    title = "Title String"
    subtitle = "(and (the) subtitle)"
    poet = "poetstring"
    composer = "compozeur"
    instrument = "instrum"
    piece = "stukkie"

    makeTitle = \markup {
	\column <
	    { "<-LEFT" \hspace #30 "centre" \hspace #30 "RIGHT->" }
            " "
	    \center < { \huge \bigger \bold \title } >
            " "
            \center <
                \center < { \normalsize \bold \subtitle } >
            >
            " "
            " "
	    { \left-align { \smaller \upright \instrument }
              \right-align { \upright \composer } }
            " "
	    { \left-align { \smaller \caps \piece }
              \right-align { \upright \poet } }
	 >
    }
}

\score {
    \context Staff \notes \relative c' {
	c-\markup { \center < \roman \caps "foe" > }
    }
}
