\version "2.25.12"

\header {
  lsrtags = "rhythms, tweaks-and-overrides"

  texidoc = "
Sometimes, a time signature should not print the whole fraction (for
example, 7/4), but only the numerator (digit@tie{}7 in this case). This
can be easily done by using
@code{\\override Staff.TimeSignature.style = #'single-number} to change
the style permanently. By using
@code{\\revert Staff.TimeSignature.style}, this setting can be
reversed. To apply the single-number style to only one time signature,
use the @code{\\override} command and prefix it with a @code{\\once}.
"

  doctitle = "Time signature printing only the numerator as a number (instead of the fraction)"
}


\relative c'' {
  \time 3/4
  c4 c c
  % Change the style permanently
  \override Staff.TimeSignature.style = #'single-number
  \time 2/4
  c4 c
  \time 3/4
  c4 c c
  % Revert to default style:
  \revert Staff.TimeSignature.style
  \time 2/4
  c4 c
  % single-number style only for the next time signature
  \once \override Staff.TimeSignature.style = #'single-number
  \time 5/4
  c4 c c c c
  \time 2/4
  c4 c
}
