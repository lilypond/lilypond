\version "2.12.0"

\header{
  texidoc="
Gregorian chant notation sometimes also uses commas and ticks, but in
smaller font size (we call it 'virgula' and 'caesura').  However, the
most common breathing signs are divisio minima/maior/maxima and
finalis, the latter three looking similar to bar glyphs.

" }

\include "gregorian.ly"

\context VaticanaStaff {
  \relative c' {
    % here is no \breathe
    c g c

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
}
