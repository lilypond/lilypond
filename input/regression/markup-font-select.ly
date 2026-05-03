\header {
  texidoc = "@code{\\font-select} in markup may be nested and does not
change fonts not listed in its argument."
}

\version "2.27.1"

%{
You may have to install additional fonts.

Red Hat Fedora

  dejavu-fonts-all

Debian GNU/Linux, Ubuntu

  fonts-dejavu-core
  fonts-dejavu-extra
%}

test-families = \markup \column {
  \serif serif-font
  \sans sans-serif-font
  \typewriter typewriter-font
}

{
  g'''4^\markup { Default: \test-families }
  g4_\markup {
    Override sans and typewriter:
    \font-select #'((sans . "DejaVu Sans")
                    (typewriter . "DejaVu Sans Mono"))
    \test-families
  }
  g''2^\markup {
    Nested override for all families:
    \font-select #'((sans . "DejaVu Sans")
                    (typewriter . "DejaVu Sans Mono"))
    \font-select #'((serif . "DejaVu Serif"))
    \test-families
  }
}
