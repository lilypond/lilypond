\version "2.25.1"

\header {
  texidoc = "The @code{\\image} markup command
supports PNG and EPS images.  The @code{background-color}
property can be set, defaulting to a white background."
}

\markup \column {
  %% TODO: support for general background color
  %% (see https://lsr.di.unimi.it/LSR/Item?id=699)
  \with-outline ""
    \with-color "#F66"
    \filled-box #'(-100 . 1000) #'(-1000 . 1000) #0

  "PNG image, white background (default):"
  \image #X #20 "lilypond.png"
  "PNG image, yellow background:"
  \override #'(background-color . "#FF9")
    \image #X #20 "lilypond.png"
  %% Warns in the PS backend, since this can't be achieved there.
  "PNG image, no background:"
  \override #'(background-color . #f)
    \image #X #20 "lilypond.png"

  %% EPS images in the Cairo backend cause a warning unless output
  %% is in PS or EPS, because they can't be supported.
  "EPS image, white background (default):"
    \image #X #20 "lilypond.eps"
  "EPS image, yellow background:"
  \override #'(background-color . "#FF9")
    \image #X #20 "lilypond.eps"
  "EPS image, no background:"
  \override #'(background-color . #f)
    \image #X #20 "lilypond.eps"
}
