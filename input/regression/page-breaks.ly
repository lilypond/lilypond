\version "2.23.4"

\header {

  texidoc = "Stress optimal page breaking.  This should look
    nice and even on 4 a6 pages. "

  
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
  ragged-last-bottom = ##f

  bookTitleMarkup = \markup {
    \override #'(baseline-skip . -3)
    \column {
      \box \fill-line { \teeny " " " " }
      \bookTitleMarkup
    }
  }
  oddHeaderMarkup = \markup \unless \on-first-page {
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
	\line { "Music engraving by LilyPond" #(lilypond-version) }4
	" "
      }
      \with-url
      %% todo: lilypond.org/music-engraving
      "https://lilypond.org/"
      \fill-line {
	"www.lilypond.org"
      }
    }
  }
}

#(set-default-paper-size "a6" 'portrait)

\book {    
  \score {
    \new Staff \relative {
      %% 19: ideally cramped
      %% Calculating page breaks...[6][11][16]
      %%\repeat unfold 19 { a b c d \break }
      %% 15: test even distribution
      %% Calculating page breaks...[5][9][13]
      \repeat unfold 15 { a b c d \break }
    }
  }
}
