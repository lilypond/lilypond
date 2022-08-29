\version "2.23.13"

\header{
  texidoc="
Gregorian chant notation sometimes also uses commas and ticks, but in
smaller font size (we call it `virgula' and `caesura').  However, the
most common breathing signs are divisio minima/@/maior/@/maxima and
finalis, the latter three looking similar to bar glyphs.
" }

\include "gregorian.ly"

\layout {
  \context {
    \Staff
    \remove Bar_engraver
    \remove Caesura_engraver
    \consists Divisio_engraver
    caesuraType = #'((breath . varcomma))
    caesuraTypeTransform = #caesura-to-divisio
  }

  \context {
    \Voice
    \remove Stem_engraver
  }
}

ancientmus = \relative {
    % here is no \breathe
    c' g c

    % \virgula applies rcomma, but in a smaller font
    c \virgula g c

    % \caesura applies rvarcomma, but in a smaller font
    c \caesura g c

    % \divisioMinima is a simple vertical stroke through the
    % uppermost staffline, just like the original implementation
    % of breathing signs.
    c \divisioMinima g c

    % \divisioMaior, \divisioMaxima and \finalis look like bars and
    % are vertically centered on the staff; the direction property
    % has no effect
    c \divisioMaior g c
    c \divisioMaxima g c

    % this one looks almost like a "||" type bar
    \finalis
  }

\context VaticanaStaff {
  \ancientmus
}

mus = \relative {
  \clef tenor
  \ancientmus
}

\new Staff \mus

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-2 0 2 4)
} \mus

\new Staff \with {
  \override StaffSymbol.line-count = #6
} \mus

\new Staff \with {
  \override StaffSymbol.line-count = #2
} \mus

\new Staff \with {
  \override StaffSymbol.line-positions = #'(-4 -2 2 5)
} \mus
