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

  bookTitleMarkup = \markup \box \bookTitleMarkup
  %%  oddHeaderMarkup = \markup \box {
  oddHeaderMarkup = \markup \on-the-fly #not-first-page \box {
    \column {
      \override #'(baseline-skip . -1)
      %%\fill-line { \teeny "---------------HEAD---------------" }
      \line { \oddHeaderMarkup }
    }
  }
  evenHeaderMarkup = \markup \box {
    \column {
      \override #'(baseline-skip . -1)
      %%\fill-line { \teeny "---------------HEAD---------------" }
      \line { \evenHeaderMarkup }
    }
  }
  oddFooterMarkup = \markup \box {
    \override #'(baseline-skip . -1)
    \column {
      \line { \oddFooterMarkup }
      \fill-line { \teeny "---------------FOOT---------------" }
    }
  }
}

#(set-default-paper-size "a6" 'portrait)

\book {    
  \score {
    \new Staff \relative c' {
      %% 18: ideally cramped
      \repeat unfold 19 { a b c d \break }
    }
  }
}
