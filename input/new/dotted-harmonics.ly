\version "2.11.61"

\header {
  lsrtags = "unfretted-strings,tweaks-and-overrides"
  texidoc = "Artificial harmonics using @code{\\harmonic} do not show
dots.  To override this behavior, set the context property
@code{harmonicDots}."
  doctitle = "Dotted harmonics"
}

\relative c''' {
  \time 3/4
  \key f \major
  \set harmonicDots = ##t
  <bes f'\harmonic>2. ~
  <bes f'\harmonic>4. <a e'\harmonic>8( <gis dis'\harmonic> <g d'\harmonic>)
  <fis cis'\harmonic>2.
  <bes f'\harmonic>2.
}
