\header {
  texidoc = "The default font families for text can be overridden."
}

\version "2.25.5"

%{
You may have to install additional fonts.

Red Hat Fedora

  dejavu-fonts-all

Debian GNU/Linux, Ubuntu

  fonts-dejavu-core
  fonts-dejavu-extra
%}

\paper {
  property-defaults.fonts.serif = "DejaVu Serif"
  property-defaults.fonts.sans = "DejaVu Sans"
  property-defaults.fonts.typewriter = "DejaVu Sans Mono"
}

bold-and-italic = \markup {
  \bold bold
  \italic italic
  \italic \bold { bold italic }
}

{
  g'''4^\markup { DejaVu Serif: \bold-and-italic }
  g4_\markup \sans { DejaVu Sans: \bold-and-italic }
  g''2^\markup \typewriter { DejaVu Sans Mono: \bold-and-italic }
}
