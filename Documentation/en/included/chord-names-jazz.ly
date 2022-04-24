\version "2.16.0"
\header {

texidoc = "This list demonstrates the support for
jazz chord names, using Klaus Ignatzek's notation."

}

chs = \transpose c' c' {
  <c e g>1
  <c es g>  % m = minor triad
  <c e gis>
  <c es ges>
  <c e g bes>
  <c es g bes>
  <c e g b>  % triangle = maj
  <c es ges beses>
  <c es ges b> \break
  <c e gis bes>
  <c es g b>
  <c e gis b>
  <c es ges bes>
  <c e g a>  % 6 = major triad with added sixth
  <c es g a>  % m6 = minor triad with added sixth
  <c e g bes d'>
  <c es g bes d'> \break
  <c es g bes d' f' a' >
  <c es g bes d' f' >
  <c es ges bes d' >
  <c e g bes des' >
  <c e g bes dis'>
  <c e g bes d' f'>
  <c e g bes d' fis'>
  <c e g bes d' f' a'> \break
  <c e g bes d' fis' as'>
  <c e gis bes dis'>
  <c e g bes dis' fis'>
  <c e g bes d' f' as'>
  <c e g bes des' f' as'>
  <c e g bes d' fis'>
  <c e g b d'>
  <c e g bes d' f' as'> \break
  <c e g bes des' f' as'>
  <c e g bes des' f' a'>
  <c e g b d'>
  <c e g b d' f' a'>
  <c e g b d' fis'>
  <c e g bes des' f ' a'> \break
  <c f g>
  <c f g bes>
  <c f g bes d'>
  <c e g d'>  % add9
  <c es g f'>
  <c e g b fis'>  % Lydian
  <c e g bes des' ees' fis' aes'>  % altered chord
}

\score {
  <<
  \chords { \chs }
  \new Staff \transpose c c' { \chs }
  >>
  \layout {
    \context {
      \Score
      \remove Bar_number_engraver
    }
  }
}

