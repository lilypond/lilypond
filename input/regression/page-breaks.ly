\version "2.5.13"

\header {

  texidoc = "Stress optimal page breaking.  This should look
    nice on 4 a6 pages. "

  
  copyright = "Copyright by /me"
  
  title = "Title"
  subtitle = "(and (the) subtitle)"
  subsubtitle = "Sub sub title"
  poet = "Poet"
  composer = "Composer"
  texttranslator = "Text Translator"
  meter = "Meter (huh?)"
  arranger = "Arranger"
  instrument = "Instrument"
  piece = "Piece"
  opus = "opus 0"

  
}

\paper {
  raggedlastbottom = ##f

  bookTitleMarkup = \markup {
    \override #'(baseline-skip . -3)
    \column {
      \box \fill-line { \teeny " " " " }
      \bookTitleMarkup
    }
  }
  oddHeaderMarkup = \markup \on-the-fly #not-first-page {
    \override #'(baseline-skip . -3)
    \column {
      \box \fill-line { \teeny " " " " }
      \line { \oddHeaderMarkup }
    }
  }
  evenHeaderMarkup = \markup {
    \override #'(baseline-skip . -3)
    \column {
      \box \fill-line { \teeny " " " " }
      \line { \evenHeaderMarkup }
    }
  }
  oddFooterMarkup = \markup {
    \override #'(baseline-skip . -3)
    \column {
	\oddFooterMarkup
	\box \fill-line { \teeny " " " " }
    }
  }
  
  tagline = \markup {
    \column {
      \fill-line {
	" "
	\line { "Music engraving by LilyPond" #(ly:export (lilypond-version)) }4
	" "
      }
      \with-url
      %% todo: lilypond.org/music-engraving
      #"http://lilypond.org/web/"
      \fill-line {
	"www.lilypond.org"
      }
    }
}


}

#(set-default-paper-size "a6" 'portrait)

\book {    
  \score {
    \new Staff \relative c' {
      %% 19: ideally cramped
      \repeat unfold 19 { a b c d \break }
    }
  }
}
