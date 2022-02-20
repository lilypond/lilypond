\version "2.21.0"

\header {
  texidoc = "Changing @code{global-staff-size} between consecutive
             @code{\\book}s must not impair font spacing.
             While the Pango fonts stay the same and may be re-used,
             the internal LilyPond scaling factor will not be correct
             any more.  Not only @code{\\abs-fontsize}, but even
             @code{\\fontsize} (in extreme cases) will be affected.
             The following output shows a 10pt book after a standard
             20pt book:"
}
\paper { indent = 0 left-margin = 10 line-width = 75 }
\header {
  title = \markup \abs-fontsize #10 "Changing global staff size"
  subtitle = \markup \abs-fontsize #10 "from 20pt to 10pt in the 2nd book"
  tagline = ##f
}
testMusic = {
  c'4_\markup \abs-fontsize #10 "\abs-fontsize #10 text"
     _\markup \abs-fontsize #10 \line { "\abs-fontsize #10
                                        \dynamic" \dynamic "fff" }
     ^\markup \fontsize #0 "\fontsize #0"
     ^\markup \fontsize #6 "\fontsize #6"
  d' e' f'
}


#(set-global-staff-size 20)
#(define output-suffix "standard-size")
\book { \score { \testMusic } }

% This book will be shown in the regression tests/collated files:
#(set-global-staff-size 10)
#(define output-suffix #f)
\book { \score { \testMusic } }
