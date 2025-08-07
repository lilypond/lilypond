\version "2.23.4"

\header {
  texidoc = "Stress optimal page breaking.  This should look
nice and even on three A6 pages."

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

  tagline = \markup \center-column {
    \line { Music engraving by LilyPond #(lilypond-version) }
    \with-url "https://lilypond.org/" "www.lilypond.org"
  }
}

#(set-default-paper-size "a6" 'portrait)

\book {
  \score {
    \new Staff \relative {
      % 19: ideally cramped
      % Calculating page breaks...[8][16]
      % \repeat unfold 19 { a b c d \break }

      % 15: test even distribution
      % Calculating page breaks...[8][16]
      \repeat unfold 15 { a b c d \break }
    }
  }
}
