\version "2.1.30"

\header {
    texidoc = "Stress optimal page breaking.  This should look nice on 4 a6 pages."
    copyright = "Copyright by /me"
    
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
}


\score {
    \context Staff \notes \relative c' {
	%% 16: ideally cramped
	%% 17: very bad without density
	\repeat unfold 17 { a b c d \break }
    }
    \paper {
	#(set-paper-size "a6")
	#(define page-breaking ly:optimal-page-breaks)
    }
}
